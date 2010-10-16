{- git-annex remote repositories -}

module Remotes (
	list,
	withKey,
	tryGitConfigRead
) where

import Control.Exception
import Control.Monad.State (liftIO)
import qualified Data.Map as Map
import Data.String.Utils
import List
import Maybe

import Types
import qualified GitRepo as Git
import qualified Annex
import LocationLog
import Locations
import UUID

{- Human visible list of remotes. -}
list :: [Git.Repo] -> String
list remotes = join " " $ map Git.repoDescribe remotes 

{- Cost ordered list of remotes that the LocationLog indicate may have a key. -}
withKey :: Key -> Annex [Git.Repo]
withKey key = do
	g <- Annex.gitRepo
	uuids <- liftIO $ keyLocations g key
	allremotes <- remotesByCost
	-- this only uses cached data, so may not find new remotes
	remotes <- reposByUUID allremotes uuids
	if (0 == length remotes)
		then tryharder allremotes uuids
		else return remotes
	where
		tryharder allremotes uuids = do
			-- more expensive; check each remote's config
			mayberemotes <- mapM tryGitConfigRead allremotes
			let allremotes' = catMaybes mayberemotes
			remotes' <- reposByUUID allremotes' uuids
			return remotes'

{- Cost Ordered list of remotes. -}
remotesByCost :: Annex [Git.Repo]
remotesByCost = do
	g <- Annex.gitRepo
	reposByCost $ Git.remotes g

{- Orders a list of git repos by cost. -}
reposByCost :: [Git.Repo] -> Annex [Git.Repo]
reposByCost l = do
	costpairs <- mapM costpair l
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
		else if (Git.repoIsLocal r)
			then return 100
			else return 200
	where
		config g r = Git.configGet g (configkey r) ""
		configkey r = "remote." ++ (Git.repoRemoteName r) ++ ".annex-cost"

{- The git configs for the git repo's remotes is not read on startup
 - because reading it may be expensive. This function tries to read the
 - config for a specified remote, and updates state. If successful, it
 - returns the updated git repo. -}
tryGitConfigRead :: Git.Repo -> Annex (Maybe Git.Repo)
tryGitConfigRead r = do
	if (Map.null $ Git.configMap r)
		then do
			-- configRead can fail due to IO error or
			-- for other reasons; catch all possible exceptions
			result <- liftIO $ (try (Git.configRead r)::IO (Either SomeException (Git.Repo)))
			case (result) of
				Left err -> return Nothing
				Right r' -> do
					g <- Annex.gitRepo
					let l = Git.remotes g
					let g' = Git.remotesAdd g $
						exchange l r'
					Annex.gitRepoChange g'
					return $ Just r'
		else return $ Just r
	where 
		exchange [] new = []
		exchange (old:ls) new =
			if ((Git.repoRemoteName old) == (Git.repoRemoteName new))
				then new:(exchange ls new)
				else old:(exchange ls new)
