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
	, test_get
	, test_move
	]

test_init :: Test
test_init = "git-annex init" ~: innewrepo $ do
	git_annex "init" ["-q", reponame] @? "init failed"
	e <- doesFileExist annexlog
	e @? (annexlog ++ " not created")
	c <- readFile annexlog
	isInfixOf reponame c @? annexlog ++ " does not contain repo name"
	where
		annexlog = ".git-annex/uuid.log"
		reponame = "test repo"

test_add :: Test
test_add = "git-annex add" ~: inoldrepo $ do
	writeFile annexedfile $ content annexedfile
	git_annex "add" ["-q", annexedfile] @? "add failed"
	checklink annexedfile
	checkcontent annexedfile
	checkunwritable annexedfile
	writeFile ingitfile $ content ingitfile
	Utility.boolSystem "git" ["add", ingitfile] @? "git add failed"
	Utility.boolSystem "git" ["commit", "-q", "-a", "-m", "commit"] @? "git commit failed"
	git_annex "add" ["-q", ingitfile] @? "add ingitfile should be no-op"
	checkregularfile ingitfile

test_unannex :: Test
test_unannex = "git-annex unannex" ~: intmpcopyrepo $ do
	git_annex "unannex" ["-q", annexedfile] @? "unannex failed"
	checkregularfile annexedfile
	git_annex "unannex" ["-q", annexedfile] @? "unannex failed on non-annexed file"
	checkregularfile annexedfile
	git_annex "unannex" ["-q", ingitfile] @? "unannex ingitfile should be no-op"

test_drop :: Test
test_drop = "git-annex drop" ~: intmpcopyrepo $ do
	r <- git_annex "drop" ["-q", annexedfile]
	(not r) @? "drop wrongly succeeded with no known copy of file"
	checklink annexedfile
	checkcontent annexedfile
	git_annex "drop" ["-q", "--force", annexedfile] @? "drop --force failed"
	checklink annexedfile
	checkdangling annexedfile
	checkunwritable annexedfile
	git_annex "drop" ["-q", annexedfile] @? "drop of dropped file failed"
	git_annex "drop" ["-q", ingitfile] @? "drop ingitfile should be no-op"
	checkregularfile ingitfile
	checkcontent ingitfile

test_get :: Test
test_get = "git-annex get" ~: intmpclonerepo $ do
	git_annex "get" ["-q", annexedfile] @? "get of file failed"
	checklink annexedfile
	checkcontent annexedfile
	checkunwritable annexedfile
	git_annex "get" ["-q", annexedfile] @? "get of file already here failed"
	checklink annexedfile
	checkcontent annexedfile
	checkunwritable annexedfile
	git_annex "get" ["-q", ingitfile] @? "get ingitfile should be no-op"
	checkregularfile ingitfile
	checkcontent ingitfile

test_move :: Test
test_move = "git-annex move" ~: intmpclonerepo $ do
	git_annex "move" ["-q", "--from", "origin", annexedfile] @? "move --from of file failed"
	checklink annexedfile
	checkcontent annexedfile
	checkunwritable annexedfile
	git_annex "move" ["-q", "--from", "origin", annexedfile] @? "move --from of file already here failed"
	checklink annexedfile
	checkcontent annexedfile
	checkunwritable annexedfile
	git_annex "move" ["-q", "--to", "origin", annexedfile] @? "move --to of file failed"
	checklink annexedfile
	checkdangling annexedfile
	checkunwritable annexedfile
	git_annex "move" ["-q", "--to", "origin", annexedfile] @? "move --to of file already here failed"
	checklink annexedfile
	checkdangling annexedfile
	checkunwritable annexedfile


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

innewrepo :: Assertion -> Test
innewrepo a = TestCase $ withgitrepo $ \r -> indir r a

inoldrepo :: Assertion -> Test
inoldrepo a = TestCase $ indir repodir a

intmpcopyrepo :: Assertion -> Test
intmpcopyrepo a = TestCase $ withtmpcopyrepo $ \r -> indir r a

intmpclonerepo :: Assertion -> Test
intmpclonerepo a = TestCase $ withtmpclonerepo $ \r -> indir r a

withtmpcopyrepo :: (FilePath -> Assertion) -> Assertion
withtmpcopyrepo = bracket (copyrepo repodir tmprepodir) cleanup

withtmpclonerepo :: (FilePath -> Assertion) -> Assertion
withtmpclonerepo = bracket (clonerepo repodir tmprepodir) cleanup

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
	Utility.boolSystem "git" ["init", "-q", dir] @? "git init failed"
	return dir

copyrepo :: FilePath -> FilePath -> IO FilePath
copyrepo old new = do
	cleanup new
	ensuretmpdir
	Utility.boolSystem "cp" ["-pr", old, new] @? "cp -pr failed"
	return new

-- clones are always done as local clones; we cannot test ssh clones
clonerepo :: FilePath -> FilePath -> IO FilePath
clonerepo old new = do
	cleanup new
	ensuretmpdir
	Utility.boolSystem "git" ["clone", "-q", old, new] @? "git clone failed"
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
	
checklink :: FilePath -> Assertion
checklink f = do
	s <- getSymbolicLinkStatus f
	isSymbolicLink s @? f ++ " is not a symlink"

checkregularfile :: FilePath -> Assertion
checkregularfile f = do
	s <- getSymbolicLinkStatus f
	isRegularFile s @? f ++ " is not a normal file"
	return ()

checkcontent :: FilePath -> Assertion
checkcontent f = do
	c <- readFile f
	assertEqual ("checkcontent " ++ f) c (content f)

checkunwritable :: FilePath -> Assertion
checkunwritable f = do
	r <- try $ writeFile f $ "dummy"
	case r of
		Left _ -> return () -- expected permission error
		Right _ -> assertFailure $ "was able to modify annexed file's " ++ f ++ " content"

checkdangling :: FilePath -> Assertion
checkdangling f = do
	r <- try $ readFile f
	case r of
		Left _ -> return () -- expected; dangling link
		Right _ -> assertFailure $ f ++ " was not a dangling link as expected"

tmpdir :: String	
tmpdir = ".t"

repodir :: String
repodir = tmpdir ++ "/repo"

tmprepodir :: String
tmprepodir = tmpdir ++ "/tmprepo"
	
annexedfile :: String
annexedfile = "foo"

ingitfile :: String
ingitfile = "bar"

content :: FilePath -> String		
content f
	| f == annexedfile = "annexed file content"
	| f == ingitfile = "normal file content"
	| otherwise = "unknown file " ++ f
