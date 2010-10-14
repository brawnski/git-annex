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
import AbstractTypes

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
	if ("" /= configured r)
		then return $ configured r
		else cached r
	where
		configured r = Git.configGet r "annex.uuid" ""
		cached r = do
			g <- gitAnnex
			return $ Git.configGet g (configkey r) ""
		configkey r = "remote." ++ (Git.repoRemoteName r) ++ ".annex-uuid"

{- Make sure that the repo has an annex.uuid setting. -}
prepUUID :: Annex ()
prepUUID = do
	g <- gitAnnex
	u <- getUUID g
	if ("" == u)
		then do
			uuid <- genUUID
			liftIO $ Git.run g ["config", configkey, uuid]
			-- re-read git config and update the repo's state
			u' <- liftIO $ Git.configRead g
			gitAnnexChange u'
			return ()
		else return ()

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
prettyPrintUUIDs :: [UUID] -> String
prettyPrintUUIDs uuids = 
	unwords $ map (\u -> "\tUUID "++u++"\n") uuids

