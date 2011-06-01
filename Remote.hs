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
	keyPossibilities,

	remoteTypes,
	genList,
	byName,
	nameToUUID,
	remotesWithUUID,
	remotesWithoutUUID,

	remoteLog,
	readRemoteLog,
	configSet,
	keyValToConfig,
	configToKeyVal,
	
	prop_idempotent_configEscape
) where

import Control.Monad.State (liftIO)
import Control.Monad (when, liftM, filterM)
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Char

import RemoteClass
import Types
import UUID
import qualified Annex
import Locations
import Utility
import Config
import Trust
import LocationLog

import qualified Remote.Git
import qualified Remote.S3
import qualified Remote.Bup
import qualified Remote.Directory
import qualified Remote.Rsync
import qualified Remote.Hook

remoteTypes :: [RemoteType Annex]
remoteTypes =
	[ Remote.Git.remote
	, Remote.S3.remote
	, Remote.Bup.remote
	, Remote.Directory.remote
	, Remote.Rsync.remote
	, Remote.Hook.remote
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
		process m t = 
			enumerate t >>=
			filterM remoteNotIgnored >>=
			mapM (gen m t)
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
nameToUUID "." = getUUID =<< Annex.gitRepo -- special case for current repo
nameToUUID n = liftM uuid (byName n)

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
	allremotes <- genList
	let validremotes = remotesWithUUID allremotes validuuids

	return (sort validremotes, validtrusteduuids)


{- Filename of remote.log. -}
remoteLog :: Annex FilePath
remoteLog = do
	g <- Annex.gitRepo
	return $ gitStateDir g ++ "remote.log"

{- Adds or updates a remote's config in the log. -}
configSet :: UUID -> RemoteConfig -> Annex ()
configSet u c = do
	m <- readRemoteLog
	l <- remoteLog
	liftIO $ safeWriteFile l $ unlines $ sort $
		map toline $ M.toList $ M.insert u c m
	where
		toline (u', c') = u' ++ " " ++ (unwords $ configToKeyVal c')

{- Map of remotes by uuid containing key/value config maps. -}
readRemoteLog :: Annex (M.Map UUID RemoteConfig)
readRemoteLog = do
	l <- remoteLog
	s <- liftIO $ catch (readFile l) ignoreerror
	return $ remoteLogParse s
	where
		ignoreerror _ = return ""

remoteLogParse :: String -> M.Map UUID RemoteConfig
remoteLogParse s =
	M.fromList $ catMaybes $ map parseline $ filter (not . null) $ lines s
	where
		parseline l
			| length w > 2 = Just (u, c)
			| otherwise = Nothing
			where
				w = words l
				u = w !! 0
				c = keyValToConfig $ tail w

{- Given Strings like "key=value", generates a RemoteConfig. -}
keyValToConfig :: [String] -> RemoteConfig
keyValToConfig ws = M.fromList $ map (/=/) ws
	where
		(/=/) s = (k, v)
			where
				k = takeWhile (/= '=') s
				v = configUnEscape $ drop (1 + length k) s

configToKeyVal :: M.Map String String -> [String]
configToKeyVal m = map toword $ sort $ M.toList m
	where
		toword (k, v) = k ++ "=" ++ configEscape v

configEscape :: String -> String
configEscape = (>>= escape)
	where
		escape c
			| isSpace c || c `elem` "&" = "&" ++ show (ord c) ++ ";"
			| otherwise = [c]

configUnEscape :: String -> String
configUnEscape = unescape
	where
		unescape [] = []
		unescape (c:rest)
			| c == '&' = entity rest
			| otherwise = c : unescape rest
		entity s = if ok
				then chr (read num) : unescape rest
				else '&' : unescape s
			where
				num = takeWhile isNumber s
				r = drop (length num) s
				rest = drop 1 r
				ok = not (null num) && 
					not (null r) && r !! 0 == ';'

{- for quickcheck -}
prop_idempotent_configEscape :: String -> Bool
prop_idempotent_configEscape s = s == (configUnEscape $ configEscape s)
