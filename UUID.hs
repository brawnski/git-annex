{- git-annex uuids
 -
 - Each git repository used by git-annex has an annex.uuid setting that
 - uniquely identifies that repository.
 -
 -}

module UUID (
	UUID,
	getUUID,
	prepUUID,
	genUUID,
	reposByUUID,
	prettyPrintUUIDs
) where

import Control.Monad.State
import Maybe
import List
import System.Cmd.Utils
import System.IO
import qualified GitRepo as Git
import Types
import qualified Annex

type UUID = String

configkey="annex.uuid"

{- Generates a UUID. There is a library for this, but it's not packaged,
 - so use the command line tool. -}
genUUID :: Annex UUID
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

	let c = cached r g
	let u = uncached r
			
	if (c /= u && u /= "")
		then do
			updatecache g r u
			return u
		else return c
	where
		uncached r = Git.configGet r "annex.uuid" ""
		cached r g = Git.configGet g (cachekey r) ""
		updatecache g r u = do
			if (g /= r)
				then setConfig (cachekey r) u
				else return ()
		cachekey r = "remote." ++ (Git.repoRemoteName r) ++ ".annex-uuid"

{- Make sure that the repo has an annex.uuid setting. -}
prepUUID :: Annex ()
prepUUID = do
	g <- Annex.gitRepo
	u <- getUUID g
	if ("" == u)
		then do
			uuid <- genUUID
			setConfig configkey uuid
		else return ()

{- Changes a git config setting in both internal state and .git/config -}
setConfig :: String -> String -> Annex ()
setConfig key value = do
	g <- Annex.gitRepo
	liftIO $ Git.run g ["config", key, value]
	-- re-read git config and update the repo's state
	g' <- liftIO $ Git.configRead g
	Annex.gitRepoChange g'
	return ()

{- Filters a list of repos to ones that have listed UUIDs. -}
reposByUUID :: [Git.Repo] -> [UUID] -> Annex [Git.Repo]
reposByUUID repos uuids = do
	filterM match repos
	where
		match r = do
			u <- getUUID r
			return $ isJust $ elemIndex u uuids

{- Pretty-prints a list of UUIDs 
 - TODO: use lookup file to really show pretty names. -}
prettyPrintUUIDs :: [UUID] -> Annex String
prettyPrintUUIDs uuids = 
	return $ unwords $ map (\u -> "\tUUID "++u++"\n") uuids

