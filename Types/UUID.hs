{- git-annex UUID type
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.UUID where

-- might be nice to have a newtype, but lots of stuff treats uuids as strings
type UUID = String
