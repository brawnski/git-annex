{- git-annex remotes overflow (can't go in there due to dependency cycles)
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module RemoteUtils where

import Control.Monad.State (liftIO)
import Data.List

import Annex
import Trust
import Remote
import UUID
import LocationLog
import Key

{- Cost ordered lists of remotes that the LocationLog indicate may have a key.
 -
 - Also returns a list of UUIDs that are trusted to have the key
 - (some may not have configured remotes).
 -}
keyPossibilities :: Key -> Annex ([Remote Annex], [UUID])
keyPossibilities key = do
	g <- Annex.gitRepo
	u <- getUUID g
	trusted <- trustGet Trusted

	-- get uuids of all remotes that are recorded to have the key
	uuids <- liftIO $ keyLocations g key
	let validuuids = filter (/= u) uuids

	-- note that validuuids is assumed to not have dups
	let validtrusteduuids = intersect validuuids trusted

	-- remotes that match uuids that have the key
	allremotes <- genList
	let validremotes = remotesWithUUID allremotes validuuids

	return (sort validremotes, validtrusteduuids)
