{- git-annex remotes class
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module RemoteClass where

import Control.Exception

import Annex
import UUID
import Key

data Remote = Remote {
	-- each Remote has a unique uuid
	uuid :: UUID,
	-- each Remote has a human visible name
	name :: String,
	-- Remotes have a use cost; higher is more expensive
	cost :: Int,
	-- Transfers a key to the remote.
	storeKey :: Key -> Annex Bool,
	-- retrieves a key's contents to a file
	retrieveKeyFile :: Key -> FilePath -> Annex Bool,
	-- removes a key's contents
	removeKey :: Key -> Annex Bool,
	-- Checks if a key is present in the remote; if the remote
	-- cannot be accessed returns a Left error.
	hasKey :: Key -> Annex (Either IOException Bool),
	-- Some remotes can check hasKey without an expensive network
	-- operation.
	hasKeyCheap :: Bool
}

instance Show Remote where
	show remote = "Remote { uuid =\"" ++ uuid remote ++ "\" }"

-- two remotes are the same if they have the same uuid
instance Eq Remote where
	a == b = uuid a == uuid b

-- order remotes by cost
instance Ord Remote where
	compare a b = compare (cost a) (cost b)
