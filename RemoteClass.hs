{- git-annex remotes class
 -
 - Most things should not need this, using Remote instead
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module RemoteClass where

import Control.Exception
import Data.Map as M

import Key

{- A remote generator identifies configured remotes, and returns an action
 - that can be run to set up each remote, and a list of names of remotes
 - that are not cheap to set up. -}
type RemoteGenerator a = ([a (Remote a)], [String])

{- There are different types of remotes. -}
data RemoteType a = RemoteType {
	-- human visible type name
	typename :: String,
	-- generates remotes of this type
	generator :: a (RemoteGenerator a),
	-- initializes or changes a remote
	setup :: String -> M.Map String String -> a (M.Map String String)
}

{- An individual remote. -}
data Remote a = Remote {
	-- each Remote has a unique uuid
	uuid :: String,
	-- each Remote has a human visible name
	name :: String,
	-- Remotes have a use cost; higher is more expensive
	cost :: Int,
	-- Transfers a key to the remote.
	storeKey :: Key -> a Bool,
	-- retrieves a key's contents to a file
	retrieveKeyFile :: Key -> FilePath -> a Bool,
	-- removes a key's contents
	removeKey :: Key -> a Bool,
	-- Checks if a key is present in the remote; if the remote
	-- cannot be accessed returns a Left error.
	hasKey :: Key -> a (Either IOException Bool),
	-- Some remotes can check hasKey without an expensive network
	-- operation.
	hasKeyCheap :: Bool,
	-- a Remote can have a persistent configuration store
	config :: Maybe (M.Map String String)
}

instance Show (Remote a) where
	show remote = "Remote { uuid =\"" ++ uuid remote ++ "\" }"

-- two remotes are the same if they have the same uuid
instance Eq (Remote a) where
	x == y = uuid x == uuid y

-- order remotes by cost
instance Ord (Remote a) where
	compare x y = compare (cost x) (cost y)
