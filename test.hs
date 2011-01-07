{- git-annex test suite
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

import Test.HUnit
import Test.HUnit.Tools
import System.Directory
import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.Files
import IO (bracket_, bracket)
import Control.Monad (unless, when)
import Data.List
import System.IO.Error

import qualified GitRepo as Git
import qualified Locations
import qualified Utility
import qualified TypeInternals
import qualified GitAnnex
import qualified CmdLine

main :: IO (Counts, Int)
main = runVerboseTests $ TestList [quickchecks, toplevels]

quickchecks :: Test
quickchecks = TestLabel "quickchecks" $ TestList
	[ qctest "prop_idempotent_deencode" Git.prop_idempotent_deencode
	, qctest "prop_idempotent_fileKey" Locations.prop_idempotent_fileKey
	, qctest "prop_idempotent_key_read_show" TypeInternals.prop_idempotent_key_read_show
	, qctest "prop_idempotent_shellEscape" Utility.prop_idempotent_shellEscape
	, qctest "prop_idempotent_shellEscape_multiword" Utility.prop_idempotent_shellEscape_multiword
	, qctest "prop_parentDir_basics" Utility.prop_parentDir_basics
	, qctest "prop_relPathDirToDir_basics" Utility.prop_relPathDirToDir_basics
	]

toplevels :: Test
toplevels = TestLabel "toplevel" $ TestList
	[ test_init
	, test_add
	]

test_init :: Test
test_init = TestLabel "git-annex init" $ TestCase $ ingitrepo $ do
	git_annex "init" ["-q", reponame] @? "init failed"
	e <- doesFileExist annexlog
	unless e $
		assertFailure $ annexlog ++ " not created"
	c <- readFile annexlog
	unless (isInfixOf reponame c) $
		assertFailure $ annexlog ++ " does not contain repo name"
	where
		annexlog = ".git-annex/uuid.log"
		reponame = "test repo"

test_add :: Test
test_add = TestLabel "git-annex add" $ TestCase $ inannex $ do
	writeFile file content
	git_annex "add" ["-q", "foo"] @? "add failed"
	s <- getSymbolicLinkStatus file
        unless (isSymbolicLink s) $
		assertFailure "git-annex add did not create symlink"
	c <- readFile file
	unless (c == content) $
		assertFailure "file content changed during git-annex add"
	r <- try (writeFile file $ content++"bar")
	case r of
		Left _ -> return () -- expected permission error
		Right _ -> assertFailure "was able to modify annexed file content"
	where
		file = "foo"
		content = "foo file content"

git_annex :: String -> [String] -> IO Bool
git_annex command params = do
	gitrepo <- Git.repoFromCwd
	r <- try $
		CmdLine.dispatch gitrepo (command:params)
			GitAnnex.cmds GitAnnex.options GitAnnex.header
	case r of
		Right _ -> return True
		Left _ -> return False

inannex :: Assertion -> Assertion
inannex a = ingitrepo $ do
	git_annex "init" ["-q", reponame] @? "init failed"
	a
	where
		reponame = "test repo"

ingitrepo :: Assertion -> Assertion
ingitrepo a = withgitrepo $ \r -> do
	cwd <- getCurrentDirectory
	bracket_ (changeWorkingDirectory $ Git.workTree r)
		(\_ -> changeWorkingDirectory cwd)
		a

withgitrepo :: (Git.Repo -> Assertion) -> Assertion
withgitrepo = bracket setup cleanup
	where
		tmpdir = ".t"
		repodir = tmpdir ++ "/repo"
		setup = do
			cleanup True
			createDirectory tmpdir
			ok <- Utility.boolSystem "git" ["init", "-q", repodir]
			unless ok $
				assertFailure "git init failed"
			return $ Git.repoFromPath repodir
		cleanup _ = do
			e <- doesDirectoryExist tmpdir
			when e $ do
				-- git-annex prevents annexed file content
				-- from being removed with permissions
				-- bits; undo
				_ <- Utility.boolSystem "chmod" ["+rw", "-R", tmpdir]
				removeDirectoryRecursive tmpdir
