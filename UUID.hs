{- git-annex uuids
 -
 - Each git repository used by git-annex has an annex.uuid setting that
 - uniquely identifies that repository.
 -
 -}

module UUID (
	getUUID,
	prepUUID,
	genUUID
) where

import System.Cmd.Utils
import System.IO
import GitRepo

type UUID = String

configkey="annex.uuid"

{- Generates a UUID. There is a library for this, but it's not packaged,
 - so use the command line tool. -}
genUUID :: IO UUID
genUUID = do
	pOpen ReadFromPipe "uuid" ["-m"] $ \h -> hGetLine h

{- Looks up a repo's UUID -}
getUUID :: GitRepo -> UUID
getUUID repo = gitConfig repo "annex.uuid" ""

{- Make sure that the repo has an annex.uuid setting. -}
prepUUID :: GitRepo -> IO GitRepo
prepUUID repo =
	if ("" == getUUID repo)
		then do
			uuid <- genUUID
			gitRun repo ["config", configkey, uuid]
			-- return new repo with updated config
			gitConfigRead repo
		else return repo
