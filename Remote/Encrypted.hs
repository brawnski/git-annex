{- common functions for encrypted remotes
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Encrypted where

import qualified Data.Map as M
import Control.Monad.State (liftIO)

import Types
import RemoteClass
import Crypto

{- Encryption setup for a remote. The user must specify whether to use
 - an encryption key, or not encrypt. An encrypted cipher is created, or is
 - updated to be accessible to an additional encryption key. -}
encryptionSetup :: RemoteConfig -> Annex RemoteConfig
encryptionSetup c =
	case (M.lookup "encryption" c, extractCipher c) of
		(Nothing, Nothing) -> error "Specify encryption=key or encryption=none"
		(Just "none", _) -> return c
		(Nothing, Just _) -> return c
		(Just _, Nothing) -> use $ genCipher c
		(Just _, Just v) -> use $ updateCipher c v
	where
		use a = do
			cipher <- liftIO a
			return $ M.delete "encryption" $ storeCipher c cipher
