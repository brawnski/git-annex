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
	prepUUID,
	genUUID,
	reposByUUID,
	prettyPrintUUIDs,
	describeUUID,
	uuidLog
) where

import Control.Monad.State
import Data.Maybe
import Data.List
import System.Cmd.Utils
import System.IO
import System.Directory
import qualified Data.Map as M
import System.Posix.Process

import qualified GitRepo as Git
import Types
import Locations
import qualified Annex
import Utility

type UUID = String

configkey :: String
configkey="annex.uuid"

{- Generates a UUID. There is a library for this, but it's not packaged,
 - so use the command line tool. -}
genUUID :: IO UUID
genUUID = liftIO $ pOpen ReadFromPipe "uuid" ["-m"] $ \h -> hGetLine h

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
	let u = uncached
	
	if c /= u && u /= ""
		then do
			updatecache g u
			return u
		else return c
	where
		uncached = Git.configGet r "annex.uuid" ""
		cached g = Git.configGet g cachekey ""
		updatecache g u = when (g /= r) $ Annex.setConfig cachekey u
		cachekey = "remote." ++ Git.repoRemoteName r ++ ".annex-uuid"

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
			return $ isJust $ elemIndex u uuids

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
	pid <- liftIO $ getProcessID
        let tmplogfile = logfile ++ ".tmp" ++ show pid
	liftIO $ createDirectoryIfMissing True (parentDir logfile)
	liftIO $ writeFile tmplogfile $ serialize m'
	liftIO $ renameFile tmplogfile logfile
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
uuidLog :: Annex String
uuidLog = do
	g <- Annex.gitRepo
	return $ gitStateDir g ++ "uuid.log"
