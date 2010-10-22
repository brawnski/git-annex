{- git-annex remote repositories -}

module Remotes (
	list,
	withKey,
	tryGitConfigRead
) where

import Control.Exception
import Control.Monad.State (liftIO)
import Control.Monad (filterM)
import qualified Data.Map as Map
import Data.String.Utils
import Data.Either.Utils
import List
import Maybe

import Types
import qualified GitRepo as Git
import qualified Annex
import LocationLog
import Locations
import UUID
import Core

{- Human visible list of remotes. -}
list :: [Git.Repo] -> String
list remotes = join " " $ map Git.repoDescribe remotes 

{- Cost ordered list of remotes that the LocationLog indicate may have a key. -}
withKey :: Key -> Annex [Git.Repo]
withKey key = do
	g <- Annex.gitRepo
	uuids <- liftIO $ keyLocations g key
	allremotes <- remotesByCost
	-- To determine if a remote has a key, its UUID needs to be known.
	-- The locally cached UIIDs of remotes can fall out of date if
	-- eg, a different drive is mounted at the same location.
	-- But, reading the config of remotes can be expensive, so make
	-- sure we only do it once per git-annex run.
	remotesread <- Annex.flagIsSet "remotesread"
	if (remotesread)
		then reposByUUID allremotes uuids
		else do
			-- We assume that it's cheap to read the config
			-- of non-URL remotes, so that is done each time.
			-- But reading the config of an URL remote is
			-- only done when there is no cached UUID value.
			let cheap = filter (not . Git.repoIsUrl) allremotes
			let expensive = filter Git.repoIsUrl allremotes
			doexpensive <- filterM cachedUUID expensive
			if (0 < length doexpensive)
				then showNote $ "getting UUIDs for " ++ (list doexpensive) ++ "..."
				else return ()
			let todo = cheap ++ doexpensive
			if (0 < length todo)
				then do
					e <- mapM tryGitConfigRead todo
					Annex.flagChange "remotesread" $ FlagBool True
					withKey key
				else reposByUUID allremotes uuids
	where
		cachedUUID r = do
			u <- getUUID r
			return $ 0 == length u 

{- Cost Ordered list of remotes. -}
remotesByCost :: Annex [Git.Repo]
remotesByCost = do
	g <- Annex.gitRepo
	reposByCost $ Git.remotes g

{- Orders a list of git repos by cost, and throws out ignored ones. -}
reposByCost :: [Git.Repo] -> Annex [Git.Repo]
reposByCost l = do
	notignored <- filterM repoNotIgnored l
	costpairs <- mapM costpair notignored
	return $ fst $ unzip $ sortBy bycost $ costpairs
	where
		costpair r = do
			cost <- repoCost r
			return (r, cost)
		bycost (_, c1) (_, c2) = compare c1 c2

{- Calculates cost for a repo.
 -
 - The default cost is 100 for local repositories, and 200 for remote
 - repositories; it can also be configured by remote.<name>.annex-cost
 -}
repoCost :: Git.Repo -> Annex Int
repoCost r = do
	g <- Annex.gitRepo
	if ((length $ config g r) > 0)
		then return $ read $ config g r
		else if (Git.repoIsUrl r)
			then return 200
			else return 100
	where
		config g r = Git.configGet g (configkey r) ""
		configkey r = "remote." ++ (Git.repoRemoteName r) ++ ".annex-cost"

{- Checks if a repo should be ignored. -}
repoNotIgnored :: Git.Repo -> Annex Bool
repoNotIgnored r = do
	g <- Annex.gitRepo
	return ("true" /= config g r)
	where
		config g r = Git.configGet g (configkey r) ""
		configkey r = "remote." ++ (Git.repoRemoteName r) ++ ".annex-ignore"

{- The git configs for the git repo's remotes is not read on startup
 - because reading it may be expensive. This function tries to read the
 - config for a specified remote, and updates state. If successful, it
 - returns the updated git repo. -}
tryGitConfigRead :: Git.Repo -> Annex (Either Git.Repo Git.Repo)
tryGitConfigRead r = do
	if (Map.null $ Git.configMap r)
		then do
			-- configRead can fail due to IO error or
			-- for other reasons; catch all possible exceptions
			result <- liftIO $ (try (Git.configRead r)::IO (Either SomeException (Git.Repo)))
			case (result) of
				Left e -> return $ Left r
				Right r' -> do
					g <- Annex.gitRepo
					let l = Git.remotes g
					let g' = Git.remotesAdd g $
						exchange l r'
					Annex.gitRepoChange g'
					return $ Right r'
		else return $ Right r -- config already read
	where 
		exchange [] new = []
		exchange (old:ls) new =
			if ((Git.repoRemoteName old) == (Git.repoRemoteName new))
				then new:(exchange ls new)
				else old:(exchange ls new)
