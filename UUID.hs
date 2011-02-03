{- git-annex uuids
 -
 - Each git repository used by git-annex has an annex.uuid setting that
 - uniquely identifies that repository.
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module UUID (
	UUID,
	getUUID,
	getUncachedUUID,
	prepUUID,
	genUUID,
	reposByUUID,
	reposWithoutUUID,
	prettyPrintUUIDs,
	describeUUID,
	uuidLog,
	uuidMap
) where

import Control.Monad.State
import System.Cmd.Utils
import System.IO
import qualified Data.Map as M

import qualified GitRepo as Git
import Types
import Locations
import qualified Annex
import Utility
import qualified SysConfig

type UUID = String

configkey :: String
configkey="annex.uuid"

{- Generates a UUID. There is a library for this, but it's not packaged,
 - so use the command line tool. -}
genUUID :: IO UUID
genUUID = liftIO $ pOpen ReadFromPipe command params $ \h -> hGetLine h
	where
		command = SysConfig.uuid
		params = if (command == "uuid")
			-- request a random uuid be generated
			then ["-m"]
			-- uuidgen generates random uuid by default
			else []

{- Looks up a repo's UUID. May return "" if none is known.
 -
 - UUIDs of remotes are cached in git config, using keys named
 - remote.<name>.annex-uuid
 -
 - -}
getUUID :: Git.Repo -> Annex UUID
getUUID r = do
	g <- Annex.gitRepo

	let c = cached g
	let u = getUncachedUUID r
	
	if c /= u && u /= ""
		then do
			updatecache g u
			return u
		else return c
	where
		cached g = Git.configGet g cachekey ""
		updatecache g u = when (g /= r) $ Annex.setConfig cachekey u
		cachekey = "remote." ++ Git.repoRemoteName r ++ ".annex-uuid"

getUncachedUUID :: Git.Repo -> UUID
getUncachedUUID r = Git.configGet r "annex.uuid" ""

{- Make sure that the repo has an annex.uuid setting. -}
prepUUID :: Annex ()
prepUUID = do
	g <- Annex.gitRepo
	u <- getUUID g
	when ("" == u) $ do
		uuid <- liftIO $ genUUID
		Annex.setConfig configkey uuid

{- Filters a list of repos to ones that have listed UUIDs. -}
reposByUUID :: [Git.Repo] -> [UUID] -> Annex [Git.Repo]
reposByUUID repos uuids = filterM match repos
	where
		match r = do
			u <- getUUID r
			return $ u `elem` uuids

{- Filters a list of repos to ones that do not have the listed UUIDs. -}
reposWithoutUUID :: [Git.Repo] -> [UUID] -> Annex [Git.Repo]
reposWithoutUUID repos uuids = filterM unmatch repos
	where
		unmatch r = do
			u <- getUUID r
			return $ u `notElem` uuids

{- Pretty-prints a list of UUIDs -}
prettyPrintUUIDs :: [UUID] -> Annex String
prettyPrintUUIDs uuids = do
	m <- uuidMap
	return $ unwords $ map (\u -> "\t" ++ prettify m u ++ "\n") uuids
	where
		prettify m u =
			if not $ null $ findlog m u
				then u ++ "  -- " ++ findlog m u
				else u
		findlog m u = M.findWithDefault "" u m

{- Records a description for a uuid in the uuidLog. -}
describeUUID :: UUID -> String -> Annex ()
describeUUID uuid desc = do
	m <- uuidMap
	let m' = M.insert uuid desc m
	logfile <- uuidLog
	liftIO $ safeWriteFile logfile (serialize m')
	where
		serialize m = unlines $ map (\(u, d) -> u++" "++d) $ M.toList m

{- Read and parse the uuidLog into a Map -}
uuidMap :: Annex (M.Map UUID String)
uuidMap = do
	logfile <- uuidLog
	s <- liftIO $ catch (readFile logfile) ignoreerror
	return $ M.fromList $ map pair $ lines s
	where
		pair l =
			if 1 < length (words l)
				then (head $ words l, unwords $ drop 1 $ words l)
				else ("", "")
		ignoreerror _ = return ""

{- Filename of uuid.log. -}
uuidLog :: Annex FilePath
uuidLog = do
	g <- Annex.gitRepo
	return $ gitStateDir g ++ "uuid.log"
