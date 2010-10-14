{- git-annex remote repositories -}

module Remotes (
	remotesList,
	remotesWithKey,
	remoteEnsureGitConfigRead
) where

import Control.Monad.State (liftIO)
import qualified Data.Map as Map
import Types
import GitRepo
import LocationLog
import Data.String.Utils
import UUID
import List

{- Human visible list of remotes. -}
remotesList :: [GitRepo] -> String
remotesList remotes = join " " $ map gitRepoDescribe remotes 

{- Cost ordered list of remotes that the LocationLog indicate may have a key. -}
remotesWithKey :: Key -> Annex [GitRepo]
remotesWithKey key = do
	g <- gitAnnex
	uuids <- liftIO $ keyLocations g key
	remotes <- remotesByCost
	reposByUUID remotes uuids

{- Cost Ordered list of remotes. -}
remotesByCost :: Annex [GitRepo]
remotesByCost = do
	g <- gitAnnex
	reposByCost $ gitRepoRemotes g

{- Orders a list of git repos by cost. -}
reposByCost :: [GitRepo] -> Annex [GitRepo]
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
repoCost :: GitRepo -> Annex Int
repoCost r = do
	g <- gitAnnex
	if ((length $ config g r) > 0)
		then return $ read $ config g r
		else if (gitRepoIsLocal r)
			then return 100
			else return 200
	where
		config g r = gitConfig g (configkey r) ""
		configkey r = "remote." ++ (gitRepoRemoteName r) ++ ".annex-cost"

{- The git configs for the git repo's remotes is not read on startup
 - because reading it may be expensive. This function ensures that it is
 - read for a specified remote, and updates state. It returns the
 - updated git repo also. -}
remoteEnsureGitConfigRead :: GitRepo -> Annex GitRepo
remoteEnsureGitConfigRead r = do
	if (Map.null $ gitConfigMap r)
		then do
			r' <- liftIO $ gitConfigRead r
			g <- gitAnnex
			let l = gitRepoRemotes g
			let g' = gitRepoRemotesAdd g $ exchange l r'
			gitAnnexChange g'
			return r'
		else return r
	where 
		exchange [] new = []
		exchange (old:ls) new =
			if ((gitRepoRemoteName old) == (gitRepoRemoteName new))
				then new:(exchange ls new)
				else old:(exchange ls new)
