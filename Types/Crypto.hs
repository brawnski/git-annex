{- git-annex crypto types
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Crypto where

import Data.String.Utils

-- XXX ideally, this would be a locked memory region
newtype Cipher = Cipher String

data EncryptedCipher = EncryptedCipher String KeyIds

newtype KeyIds = KeyIds [String]

instance Show KeyIds where
	show (KeyIds ks) = join "," ks

instance Read KeyIds where
	readsPrec _ s = [(KeyIds (split "," s), "")]
