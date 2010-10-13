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
	reposByUUID
) where

import Maybe
import List
import System.Cmd.Utils
import System.IO
import GitRepo
import Types

type UUID = String

configkey="annex.uuid"

{- Generates a UUID. There is a library for this, but it's not packaged,
 - so use the command line tool. -}
genUUID :: IO UUID
genUUID = do
	pOpen ReadFromPipe "uuid" ["-m"] $ \h -> hGetLine h

{- Looks up a repo's UUID. May return "" if none is known.
 -
 - UUIDs of remotes are cached in git config, using keys named
 - remote.<name>.annex-uuid
 -
 - -}
getUUID :: State -> GitRepo -> UUID
getUUID s r = 
	if ("" /= getUUID' r)
		then getUUID' r
		else cached s r
	where
		cached s r = gitConfig (repo s) (configkey r) ""
		configkey r = "remote." ++ (gitRepoRemoteName r) ++ ".annex-uuid"
getUUID' r = gitConfig r "annex.uuid" ""

{- Make sure that the repo has an annex.uuid setting. -}
prepUUID :: GitRepo -> IO GitRepo
prepUUID repo =
	if ("" == getUUID' repo)
		then do
			uuid <- genUUID
			gitRun repo ["config", configkey, uuid]
			-- return new repo with updated config
			gitConfigRead repo
		else return repo

{- Filters a list of repos to ones that have listed UUIDs. -}
reposByUUID :: State -> [GitRepo] -> [UUID] -> [GitRepo]
reposByUUID state repos uuids =
	filter (\r -> isJust $ elemIndex (getUUID state r) uuids) repos
