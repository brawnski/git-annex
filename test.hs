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
import qualified Control.Exception.Extensible as E

import qualified GitRepo as Git
import qualified Locations
import qualified Utility
import qualified TypeInternals
import qualified GitAnnex
import qualified CmdLine

main :: IO (Counts, Int)
main = do
	r <- runVerboseTests $ TestList [quickchecks, toplevels]
	cleanup tmpdir
	return r

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
	-- test order matters, later tests may rely on state from earlier
	[ test_init
	, test_add
	, test_unannex
	, test_drop
	]

test_init :: Test
test_init = TestLabel "git-annex init" $ TestCase $ innewrepo $ do
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
test_add = TestLabel "git-annex add" $ TestCase $ inoldrepo $ do
	writeFile file content
	git_annex "add" ["-q", "foo"] @? "add failed"
	s <- getSymbolicLinkStatus file
        unless (isSymbolicLink s) $
		assertFailure "git-annex add did not create symlink"
	c <- readFile file
	unless (c == content) $
		assertFailure "file content changed during git-annex add"
	r <- try $ writeFile file $ content++"bar"
	case r of
		Left _ -> return () -- expected permission error
		Right _ -> assertFailure "was able to modify annexed file content"
	where
		file = "foo"
		content = "foo file content"

test_unannex :: Test
test_unannex = TestLabel "git-annex unannex" $ TestCase $ intmpcopyrepo $ do
	git_annex "unannex" ["-q", "foo"] @? "unannex failed"
	s <- getSymbolicLinkStatus "foo"
	when (isSymbolicLink s) $
		assertFailure "git-annex unannex left symlink"

test_drop :: Test
test_drop = TestLabel "git-annex drop" $ TestCase $ intmpcopyrepo $ do
	r <- git_annex "drop" ["-q", "foo"]
	(not r) @? "drop wrongly succeeded with no known copy of file"
	checklink
	git_annex "drop" ["-q", "--force", "foo"] @? "drop --force failed"
	checklink
	r' <- try $ readFile "foo"
	case r' of
		Left _ -> return () -- expected; dangling link
		Right _ -> assertFailure "drop did not remove file content"
	where
		checklink = do
			s <- getSymbolicLinkStatus "foo"
			unless (isSymbolicLink s) $
				assertFailure "git-annex drop killed symlink"




git_annex :: String -> [String] -> IO Bool
git_annex command params = do
	-- catch all errors, including normally fatal errors
	r <- E.try (run)::IO (Either E.SomeException ())
	case r of
		Right _ -> return True
		Left _ -> return False
	where
		run = do
			gitrepo <- Git.repoFromCwd
			CmdLine.dispatch gitrepo (command:params)
				GitAnnex.cmds GitAnnex.options GitAnnex.header

innewannex :: Assertion -> Assertion
innewannex a = innewrepo $ do
	git_annex "init" ["-q", reponame] @? "init failed"
	a
	where
		reponame = "test repo"

innewrepo :: Assertion -> Assertion
innewrepo a = withgitrepo $ \r -> indir r a

inoldrepo :: Assertion -> Assertion
inoldrepo = indir repodir

intmpcopyrepo :: Assertion -> Assertion
intmpcopyrepo a = withtmpcopyrepo $ \r -> indir r a

withtmpcopyrepo :: (FilePath -> Assertion) -> Assertion
withtmpcopyrepo = bracket (copyrepo repodir tmprepodir) cleanup

withgitrepo :: (FilePath -> Assertion) -> Assertion
withgitrepo = bracket (setuprepo repodir) return

indir :: FilePath -> Assertion -> Assertion
indir dir a = do
	cwd <- getCurrentDirectory
	bracket_ (changeWorkingDirectory $ dir)
		(\_ -> changeWorkingDirectory cwd)
		a

setuprepo :: FilePath -> IO FilePath
setuprepo dir = do
	cleanup dir
	ensuretmpdir
	ok <- Utility.boolSystem "git" ["init", "-q", dir]
	unless ok $
		assertFailure "git init failed"
	return dir

copyrepo :: FilePath -> FilePath -> IO FilePath
copyrepo old new = do
	cleanup new
	ensuretmpdir
	ok <- Utility.boolSystem "cp" ["-pr", old, new]
	unless ok $
		assertFailure "cp -pr failed"
	return new
	
ensuretmpdir :: IO ()
ensuretmpdir = do
	e <- doesDirectoryExist tmpdir
	unless e $
		createDirectory tmpdir

cleanup :: FilePath -> IO ()
cleanup dir = do
	e <- doesDirectoryExist dir
	when e $ do
		-- git-annex prevents annexed file content from being
		-- removed via permissions bits; undo
		_ <- Utility.boolSystem "chmod" ["+rw", "-R", dir]
		removeDirectoryRecursive dir

tmpdir :: String	
tmpdir = ".t"

repodir :: String
repodir = tmpdir ++ "/repo"

tmprepodir :: String
tmprepodir = tmpdir ++ "/tmprepo"
