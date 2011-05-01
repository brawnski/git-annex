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
import Config

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
		(Just _, Nothing) -> use "encryption setup" $ genCipher c
		(Just _, Just v) -> use "encryption updated" $ updateCipher c v
	where
		use m a = do
			cipher <- liftIO a
			showNote $ m ++ " " ++ describeCipher cipher
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
		hasKey = withkey $ hasKey r,
		cost = cost r + encryptedRemoteCostAdj
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

{- Gets encryption Cipher. The decrypted Cipher is cached in the Annex
 - state. -}
remoteCipher :: RemoteConfig -> Annex (Maybe Cipher)
remoteCipher c = do
	cache <- Annex.getState Annex.cipher
	case cache of
		Just cipher -> return $ Just cipher
		Nothing -> case extractCipher c of
			Nothing -> return Nothing
			Just encipher -> do
				showNote "gpg"
				cipher <- liftIO $ decryptCipher c encipher
				Annex.changeState (\s -> s { Annex.cipher = Just cipher })
				return $ Just cipher

{- Gets encryption Cipher, and encrypted version of Key. -}
cipherKey :: Maybe RemoteConfig -> Key -> Annex (Maybe (Cipher, Key))
cipherKey Nothing _ = return Nothing
cipherKey (Just c) k = do
	cipher <- remoteCipher c
	case cipher of	
		Just ciphertext -> do
			k' <- liftIO $ encryptKey ciphertext k
			return $ Just (ciphertext, k')
		Nothing -> return Nothing
