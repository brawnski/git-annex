{- git-annex remotes
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote (
	Remote,
	uuid,
	name,
	storeKey,
	retrieveKeyFile,
	removeKey,
	hasKey,
	hasKeyCheap,

	byName,
	nameToUUID,
	keyPossibilities,
	remotesWithUUID,
	remotesWithoutUUID
) where

import Control.Monad.State (liftIO)
import Control.Monad (when, liftM)
import Data.List

import RemoteClass
import qualified Remote.GitRemote
import Types
import UUID
import qualified Annex
import Trust
import LocationLog
import Messages

{- add generators for new Remotes here -}
generators :: [Annex [Remote Annex]]
generators = [Remote.GitRemote.generate]

{- Builds a list of all available Remotes.
 - Since doing so can be expensive, the list is cached in the Annex. -}
genList :: Annex [Remote Annex]
genList = do
	g <- Annex.gitRepo
	u <- getUUID g
	showNote $ "Remote.genList " ++ u
	rs <- Annex.getState Annex.remotes
	if null rs
		then do
			lists <- sequence generators
			let rs' = concat lists
			Annex.changeState $ \s -> s { Annex.remotes = rs' }
			return rs'
		else return rs

{- Looks up a remote by name. (Or by UUID.) -}
byName :: String -> Annex (Remote Annex)
byName "" = error "no remote specified"
byName n = do
	allremotes <- genList
	let match = filter matching allremotes
	when (null match) $ error $
		"there is no git remote named \"" ++ n ++ "\""
	return $ head match
	where
		matching r = n == name r || n == uuid r

{- Looks up a remote by name (or by UUID), and returns its UUID. -}
nameToUUID :: String -> Annex UUID
nameToUUID "." = do -- special case for current repo
	g <- Annex.gitRepo
	getUUID g
nameToUUID n = liftM uuid (byName n)

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

{- Filters a list of remotes to ones that have the listed uuids. -}
remotesWithUUID :: [Remote Annex] -> [UUID] -> [Remote Annex]
remotesWithUUID rs us = filter (\r -> uuid r `elem` us) rs

{- Filters a list of remotes to ones that do not have the listed uuids. -}
remotesWithoutUUID :: [Remote Annex] -> [UUID] -> [Remote Annex]
remotesWithoutUUID rs us = filter (\r -> uuid r `notElem` us) rs

