{- git-annex key/value backend data type
 -
 - Most things should not need this, using Types instead
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module BackendClass where

import Key

data Backend a = Backend {
	-- name of this backend
	name :: String,
	-- converts a filename to a key
	getKey :: FilePath -> a (Maybe Key),
	-- stores a file's contents to a key
	storeFileKey :: FilePath -> Key -> a Bool,
	-- retrieves a key's contents to a file
	retrieveKeyFile :: Key -> FilePath -> a Bool,
	-- removes a key, optionally checking that enough copies are stored
	-- elsewhere
	removeKey :: Key -> Maybe Int -> a Bool,
	-- checks if a backend is storing the content of a key
	hasKey :: Key -> a Bool,
	-- called during fsck to check a key
	-- (second parameter may be the filename associated with it)
	-- (third parameter may be the number of copies that there should
	-- be of the key)
	fsckKey :: Key -> Maybe FilePath -> Maybe Int -> a Bool,
	-- Is a newer repesentation possible for a key?
	upgradableKey :: Key -> a Bool
}

instance Show (Backend a) where
	show backend = "Backend { name =\"" ++ name backend ++ "\" }"

instance Eq (Backend a) where
	a == b = name a == name b
