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

	remoteTypes,
	byName,
	nameToUUID,
	keyPossibilities,
	remotesWithUUID,
	remotesWithoutUUID,

	remoteLog,
	readRemoteLog,
	configSet,
	keyValToMap
) where

import Control.Monad.State (liftIO)
import Control.Monad (when, liftM, filterM)
import Data.List
import qualified Data.Map as M
import Data.Maybe

import RemoteClass
import Types
import UUID
import qualified Annex
import Trust
import LocationLog
import Locations
import Utility
import Config

import qualified Remote.Git
import qualified Remote.S3
import qualified Remote.Bup
import qualified Remote.Directory

remoteTypes :: [RemoteType Annex]
remoteTypes =
	[ Remote.Git.remote
	, Remote.S3.remote
	, Remote.Bup.remote
	, Remote.Directory.remote
	]

{- Builds a list of all available Remotes.
 - Since doing so can be expensive, the list is cached. -}
genList :: Annex [Remote Annex]
genList = do
	rs <- Annex.getState Annex.remotes
	if null rs
		then do
			m <- readRemoteLog
			l <- mapM (process m) remoteTypes
			let rs' = concat l
			Annex.changeState $ \s -> s { Annex.remotes = rs' }
			return rs'
		else return rs
	where
		process m t = do
			l <- enumerate t
			l' <- filterM remoteNotIgnored l
			mapM (gen m t) l'
		gen m t r = do
			u <- getUUID r
			generate t r u (M.lookup u m)

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

{- Filename of remote.log. -}
remoteLog :: Annex FilePath
remoteLog = do
	g <- Annex.gitRepo
	return $ gitStateDir g ++ "remote.log"

{- Adds or updates a remote's config in the log. -}
configSet :: UUID -> M.Map String String -> Annex ()
configSet u c = do
	m <- readRemoteLog
	l <- remoteLog
	liftIO $ safeWriteFile l $ unlines $ sort $
		map toline $ M.toList $ M.insert u c m
	where
		toline (u', c') = u' ++ " " ++ (unwords $ mapToKeyVal c')

{- Map of remotes by uuid containing key/value config maps. -}
readRemoteLog :: Annex (M.Map UUID (M.Map String String))
readRemoteLog = do
	l <- remoteLog
	s <- liftIO $ catch (readFile l) ignoreerror
	return $ remoteLogParse s
	where
		ignoreerror _ = return ""

remoteLogParse :: String -> M.Map UUID (M.Map String String)
remoteLogParse s =
	M.fromList $ catMaybes $ map parseline $ filter (not . null) $ lines s
	where
		parseline l
			| length w > 2 = Just (u, c)
			| otherwise = Nothing
			where
				w = words l
				u = w !! 0
				c = keyValToMap $ tail w

{- Given Strings like "key=value", generates a Map. -}
keyValToMap :: [String] -> M.Map String String
keyValToMap ws = M.fromList $ map (/=/) ws
	where
		(/=/) s = (k, v)
			where
				k = takeWhile (/= '=') s
				v = drop (1 + length k) s

mapToKeyVal :: M.Map String String -> [String]
mapToKeyVal m = map toword $ sort $ M.toList m
	where
		toword (k, v) = k ++ "=" ++ v
