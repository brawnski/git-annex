{- common functions for encryptable remotes
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Encryptable where

import qualified Data.Map as M
import Control.Monad.State (liftIO)

import Types
import RemoteClass
import Crypto
import qualified Annex
import Messages

{- Encryption setup for a remote. The user must specify whether to use
 - an encryption key, or not encrypt. An encrypted cipher is created, or is
 - updated to be accessible to an additional encryption key. -}
encryptionSetup :: RemoteConfig -> Annex RemoteConfig
encryptionSetup c =
	case (M.lookup "encryption" c, extractCipher c) of
		(Nothing, Nothing) -> error "Specify encryption=key or encryption=none"
		(Just "none", Nothing) -> return c
		(Just "none", Just _) -> error "Cannot change encryption type of existing remote."
		(Nothing, Just _) -> return c
		(Just _, Nothing) -> use $ genCipher c
		(Just _, Just v) -> use $ updateCipher c v
	where
		use a = do
			cipher <- liftIO a
			return $ M.delete "encryption" $ storeCipher c cipher

{- Modifies a Remote to support encryption.
 -
 - Two additional functions must be provided by the remote,
 - to support storing and retrieving encrypted content. -}
encryptableRemote
	:: Maybe RemoteConfig
	-> ((Cipher, Key) -> Key -> Annex Bool)
	-> ((Cipher, Key) -> FilePath -> Annex Bool)
	-> Remote Annex 
	-> Remote Annex
encryptableRemote c storeKeyEncrypted retrieveKeyFileEncrypted r = 
	r {
		storeKey = store,
		retrieveKeyFile = retrieve,
		removeKey = withkey $ removeKey r,
		hasKey = withkey $ hasKey r
	}
	where
		store k = do
			v <- cipherKey c k
			case v of
				Nothing -> (storeKey r) k
				Just x -> storeKeyEncrypted x k
		retrieve k f = do
			v <- cipherKey c k
			case v of
				Nothing -> (retrieveKeyFile r) k f
				Just x -> retrieveKeyFileEncrypted x f
		withkey a k = do
			v <- cipherKey c k
			case v of
				Nothing -> a k
				Just (_, k') -> a k'

{- Gets encryption Cipher, and encrypted version of Key. 
 -
 - The decrypted Cipher is cached in the Annex state. -}
cipherKey :: Maybe RemoteConfig -> Key -> Annex (Maybe (Cipher, Key))
cipherKey Nothing _ = return Nothing
cipherKey (Just c) k = do
	cache <- Annex.getState Annex.cipher
	case cache of
		Just cipher -> ret cipher
		Nothing -> case extractCipher c of
			Nothing -> return Nothing
			Just encipher -> do
				showNote "getting encryption key"
				cipher <- liftIO $ decryptCipher c encipher
				Annex.changeState (\s -> s { Annex.cipher = Just cipher })
				ret cipher
	where
		ret cipher = do
			k' <- liftIO $ encryptKey cipher k
			return $ Just (cipher, k')
