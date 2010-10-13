{- git-annex remote repositories -}

module Remotes (
	remotesList,
	remotesWithKey
) where

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
remotesWithKey :: State -> Key -> IO [GitRepo]
remotesWithKey state key = do
	uuids <- keyLocations (repo state) key
	return $ reposByUUID state (remotesByCost state) uuids

{- Cost Ordered list of remotes. -}
remotesByCost :: State -> [GitRepo]
remotesByCost state = reposByCost state $ gitConfigRemotes (repo state)

{- Orders a list of git repos by cost. -}
reposByCost :: State -> [GitRepo] -> [GitRepo]
reposByCost state l =
	fst $ unzip $ sortBy (\(r1, c1) (r2, c2) -> compare c1 c2) $ costpairs l
	where
		costpairs l = map (\r -> (r, repoCost state r)) l

{- Calculates cost for a repo.
 -
 - The default cost is 100 for local repositories, and 200 for remote
 - repositories; it can also be configured by remote.<name>.annex-cost
 -}
repoCost :: State -> GitRepo -> Int
repoCost state r = 
	if ((length $ config state r) > 0)
		then read $ config state r
		else if (gitRepoIsLocal r)
			then 100
			else 200
	where
		config state r = gitConfig (repo state) (configkey r) ""
		configkey r = "remote." ++ (gitRepoRemoteName r) ++ ".annex-cost"
