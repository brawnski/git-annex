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

configkey="annex.uuid"

{- Generates a UUID. There is a library for this, but it's not packaged,
 - so use the command line tool. -}
genUUID :: IO String
genUUID = do
	pOpen ReadFromPipe "uuid" ["-m"] $ \h -> hGetLine h

getUUID :: GitRepo -> String
getUUID repo = gitConfig repo "annex.uuid" ""

{- Make sure that the repo has an annex.uuid setting. -}
prepUUID :: GitRepo -> IO ()
prepUUID repo =
	if ("" == getUUID repo)
		then do
			uuid <- genUUID
			gitRun repo ["config", configkey, uuid]
		else return ()
