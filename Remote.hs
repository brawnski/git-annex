{- git-annex remotes
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote (
	generate,
	keyPossibilities,
	remotesWithUUID,
	remotesWithoutUUID
) where

import Control.Monad.State (liftIO)
import Data.List

import RemoteClass
import qualified Remote.GitRemote
import Types
import UUID
import qualified Annex
import Trust
import LocationLog

{- add generators for new Remotes here -}
generators :: [Annex [Remote Annex]]
generators = [Remote.GitRemote.generate]

{- generates a list of all available Remotes -}
generate :: Annex [Remote Annex]
generate = do
	lists <- sequence generators
	return $ concat lists

{- Filters a list of remotes to ones that have the listed uuids. -}
remotesWithUUID :: [Remote Annex] -> [UUID] -> [Remote Annex]
remotesWithUUID rs us = filter (\r -> uuid r `elem` us) rs

{- Filters a list of remotes to ones that do not have the listed uuids. -}
remotesWithoutUUID :: [Remote Annex] -> [UUID] -> [Remote Annex]
remotesWithoutUUID rs us = filter (\r -> uuid r `notElem` us) rs

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
	allremotes <- generate
	let validremotes = remotesWithUUID allremotes validuuids

	return (sort validremotes, validtrusteduuids)
