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
import System.Posix.Env
import qualified Control.Exception.Extensible as E

import qualified GitRepo as Git
import qualified Locations
import qualified Utility
import qualified TypeInternals
import qualified GitAnnex

main :: IO ()
main = do
	prepare
	r <- runVerboseTests $ TestList [quickchecks, toplevels]
	cleanup tmpdir
	propigate r

propigate :: (Counts, Int) -> IO ()
propigate (Counts { errors = e }, _)
	| e > 0 = error "failed"
	| otherwise = return ()

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
	, test_copy
	, test_lock
	, test_edit
	, test_fix
	]

test_init :: Test
test_init = "git-annex init" ~: TestCase $ innewrepo $ do
	git_annex "init" ["-q", reponame] @? "init failed"
	e <- doesFileExist annexlog
	e @? (annexlog ++ " not created")
	c <- readFile annexlog
	isInfixOf reponame c @? annexlog ++ " does not contain repo name"
	where
		annexlog = ".git-annex/uuid.log"
		reponame = "test repo"

test_add :: Test
test_add = "git-annex add" ~: TestCase $ inmainrepo $ do
	writeFile annexedfile $ content annexedfile
	git_annex "add" ["-q", annexedfile] @? "add failed"
	annexed_present annexedfile
	writeFile ingitfile $ content ingitfile
	Utility.boolSystem "git" ["add", ingitfile] @? "git add failed"
	Utility.boolSystem "git" ["commit", "-q", "-a", "-m", "commit"] @? "git commit failed"
	git_annex "add" ["-q", ingitfile] @? "add ingitfile should be no-op"
	unannexed ingitfile

test_unannex :: Test
test_unannex = "git-annex unannex" ~: TestCase $ intmpcopyrepo $ do
	git_annex "unannex" ["-q", annexedfile] @? "unannex failed"
	unannexed annexedfile
	git_annex "unannex" ["-q", annexedfile] @? "unannex failed on non-annexed file"
	unannexed annexedfile
	git_annex "unannex" ["-q", ingitfile] @? "unannex ingitfile should be no-op"
	unannexed ingitfile

test_drop :: Test
test_drop = "git-annex drop" ~: TestList [nocopy, withcopy]
	where
		nocopy = "no remotes" ~: TestCase $ intmpcopyrepo $ do
			r <- git_annex "drop" ["-q", annexedfile]
			(not r) @? "drop wrongly succeeded with no known copy of file"
			annexed_present annexedfile
			git_annex "drop" ["-q", "--force", annexedfile] @? "drop --force failed"
			annexed_notpresent annexedfile
			git_annex "drop" ["-q", annexedfile] @? "drop of dropped file failed"
			git_annex "drop" ["-q", ingitfile] @? "drop ingitfile should be no-op"
			unannexed ingitfile
		withcopy = "with remote" ~: TestCase $ intmpclonerepo $ do
			git_annex "drop" ["-q", annexedfile] @? "drop failed though origin has copy"
			annexed_notpresent annexedfile
			inmainrepo $ annexed_present annexedfile

test_get :: Test
test_get = "git-annex get" ~: TestCase $ intmpclonerepo $ do
	inmainrepo $ annexed_present annexedfile
	annexed_notpresent annexedfile
	git_annex "get" ["-q", annexedfile] @? "get of file failed"
	inmainrepo $ annexed_present annexedfile
	annexed_present annexedfile
	git_annex "get" ["-q", annexedfile] @? "get of file already here failed"
	inmainrepo $ annexed_present annexedfile
	annexed_present annexedfile
	inmainrepo $ unannexed ingitfile
	unannexed ingitfile
	git_annex "get" ["-q", ingitfile] @? "get ingitfile should be no-op"
	inmainrepo $ unannexed ingitfile
	unannexed ingitfile

test_move :: Test
test_move = "git-annex move" ~: TestCase $ intmpclonerepo $ do
	annexed_notpresent annexedfile
	inmainrepo $ annexed_present annexedfile
	git_annex "move" ["-q", "--from", "origin", annexedfile] @? "move --from of file failed"
	annexed_present annexedfile
	inmainrepo $ annexed_notpresent annexedfile
	git_annex "move" ["-q", "--from", "origin", annexedfile] @? "move --from of file already here failed"
	annexed_present annexedfile
	inmainrepo $ annexed_notpresent annexedfile
	git_annex "move" ["-q", "--to", "origin", annexedfile] @? "move --to of file failed"
	inmainrepo $ annexed_present annexedfile
	annexed_notpresent annexedfile
	git_annex "move" ["-q", "--to", "origin", annexedfile] @? "move --to of file already there failed"
	inmainrepo $ annexed_present annexedfile
	annexed_notpresent annexedfile
	unannexed ingitfile
	inmainrepo $ unannexed ingitfile
	git_annex "move" ["-q", "--to", "origin", ingitfile] @? "move of ingitfile should be no-op"
	unannexed ingitfile
	inmainrepo $ unannexed ingitfile
	git_annex "move" ["-q", "--from", "origin", ingitfile] @? "move of ingitfile should be no-op"
	unannexed ingitfile
	inmainrepo $ unannexed ingitfile

test_copy :: Test
test_copy = "git-annex copy" ~: TestCase $ intmpclonerepo $ do
	annexed_notpresent annexedfile
	inmainrepo $ annexed_present annexedfile
	git_annex "copy" ["-q", "--from", "origin", annexedfile] @? "copy --from of file failed"
	annexed_present annexedfile
	inmainrepo $ annexed_present annexedfile
	git_annex "copy" ["-q", "--from", "origin", annexedfile] @? "copy --from of file already here failed"
	annexed_present annexedfile
	inmainrepo $ annexed_present annexedfile
	git_annex "copy" ["-q", "--to", "origin", annexedfile] @? "copy --to of file already there failed"
	annexed_present annexedfile
	inmainrepo $ annexed_present annexedfile
	git_annex "move" ["-q", "--to", "origin", annexedfile] @? "move --to of file already there failed"
	annexed_notpresent annexedfile
	inmainrepo $ annexed_present annexedfile
	unannexed ingitfile
	inmainrepo $ unannexed ingitfile
	git_annex "copy" ["-q", "--to", "origin", ingitfile] @? "copy of ingitfile should be no-op"
	unannexed ingitfile
	inmainrepo $ unannexed ingitfile
	git_annex "copy" ["-q", "--from", "origin", ingitfile] @? "copy of ingitfile should be no-op"
	checkregularfile ingitfile
	checkcontent ingitfile

test_lock :: Test
test_lock = "git-annex unlock/lock" ~: intmpclonerepo $ do
	git_annex "get" ["-q", annexedfile] @? "get of file failed"
	annexed_present annexedfile
	git_annex "unlock" ["-q", annexedfile] @? "unlock failed"		
	unannexed annexedfile
	-- write different content, to verify that lock
	-- throws it away
	changecontent annexedfile
	writeFile annexedfile $ (content annexedfile) ++ "foo"
	git_annex "lock" ["-q", annexedfile] @? "lock failed"
	annexed_present annexedfile
	git_annex "unlock" ["-q", annexedfile] @? "unlock failed"		
	unannexed annexedfile
	changecontent annexedfile
	git_annex "add" ["-q", annexedfile] @? "add of modified file failed"
	runchecks [checklink, checkunwritable] annexedfile
	c <- readFile annexedfile
	assertEqual ("content of modified file") c (changedcontent annexedfile)
	r <- git_annex "drop" ["-q", annexedfile]
	(not r) @? "drop wrongly succeeded with no known copy of modified file"

test_edit :: Test
test_edit = "git-annex edit/commit" ~: intmpclonerepo $ do
	git_annex "get" ["-q", annexedfile] @? "get of file failed"
	annexed_present annexedfile
	git_annex "edit" ["-q", annexedfile] @? "edit failed"
	unannexed annexedfile
	changecontent annexedfile
	Utility.boolSystem "git" ["commit", "-q", "-a", "-m", "content changed"]
		@? "git commit of edited file failed"
	runchecks [checklink, checkunwritable] annexedfile
	c <- readFile annexedfile
	assertEqual ("content of modified file") c (changedcontent annexedfile)
	r <- git_annex "drop" ["-q", annexedfile]
	(not r) @? "drop wrongly succeeded with no known copy of modified file"

test_fix :: Test
test_fix = "git-annex fix" ~: intmpclonerepo $ do
	annexed_notpresent annexedfile
	git_annex "fix" ["-q", annexedfile] @? "fix of not present failed"
	annexed_notpresent annexedfile
	git_annex "get" ["-q", annexedfile] @? "get of file failed"
	annexed_present annexedfile
	git_annex "fix" ["-q", annexedfile] @? "fix of present file failed"
	annexed_present annexedfile
	createDirectory subdir
	Utility.boolSystem "git" ["mv", annexedfile, subdir]
		@? "git mv failed"
	git_annex "fix" ["-q", newfile] @? "fix of moved file failed"
	runchecks [checklink, checkunwritable] newfile
	c <- readFile newfile
	assertEqual ("content of moved file") c (content annexedfile)
	where
		subdir = "s"
		newfile = subdir ++ "/" ++ annexedfile

git_annex :: String -> [String] -> IO Bool
git_annex command params = do
	-- catch all errors, including normally fatal errors
	r <- E.try (run)::IO (Either E.SomeException ())
	case r of
		Right _ -> return True
		Left _ -> return False
	where
		run = GitAnnex.run (command:params)

innewrepo :: Assertion -> Assertion
innewrepo a = withgitrepo $ \r -> indir r a

inmainrepo :: Assertion -> Assertion
inmainrepo a = indir mainrepodir a

intmpcopyrepo :: Assertion -> Assertion
intmpcopyrepo a = withtmpcopyrepo $ \r -> indir r a

intmpclonerepo :: Assertion -> Assertion
intmpclonerepo a = withtmpclonerepo $ \r -> indir r a

withtmpcopyrepo :: (FilePath -> Assertion) -> Assertion
withtmpcopyrepo = bracket (copyrepo mainrepodir tmprepodir) cleanup

withtmpclonerepo :: (FilePath -> Assertion) -> Assertion
withtmpclonerepo = bracket (clonerepo mainrepodir tmprepodir) cleanup

withgitrepo :: (FilePath -> Assertion) -> Assertion
withgitrepo = bracket (setuprepo mainrepodir) return

indir :: FilePath -> Assertion -> Assertion
indir dir a = do
	cwd <- getCurrentDirectory
	bracket_ (changeToTmpDir dir)
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
	_ <- clonerepo old new
	indir new $ Utility.boolSystem "git" ["remote", "rm", "origin"] @? "git remote failed"
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
	indir new $ git_annex "init" ["-q", new] @? "git annex init failed"
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
	r <- try $ writeFile f $ content f
	case r of
		Left _ -> return () -- expected permission error
		Right _ -> assertFailure $ "was able to modify annexed file's " ++ f ++ " content"

checkwritable :: FilePath -> Assertion
checkwritable f = do
	r <- try $ writeFile f $ content f
	case r of
		Left _ -> assertFailure $ "unable to modify " ++ f
		Right _ -> return ()

checkdangling :: FilePath -> Assertion
checkdangling f = do
	r <- try $ readFile f
	case r of
		Left _ -> return () -- expected; dangling link
		Right _ -> assertFailure $ f ++ " was not a dangling link as expected"

runchecks :: [FilePath -> Assertion] -> FilePath -> Assertion
runchecks [] _ = return ()
runchecks (a:as) f = do
	a f
	runchecks as f

annexed_notpresent :: FilePath -> Assertion
annexed_notpresent = runchecks [checklink, checkdangling, checkunwritable]

annexed_present :: FilePath -> Assertion
annexed_present = runchecks [checklink, checkcontent, checkunwritable]

unannexed :: FilePath -> Assertion
unannexed = runchecks [checkregularfile, checkcontent, checkwritable]

prepare :: IO ()
prepare = do
	-- While PATH is mostly avoided, the commit hook does run it. Make
	-- sure that the just-built git annex is used.
	cwd <- getCurrentDirectory
	p <- getEnvDefault  "PATH" ""
	setEnv "PATH" (cwd ++ ":" ++ p) True
	setEnv "TOPDIR" cwd True

changeToTmpDir :: FilePath -> IO ()
changeToTmpDir t = do
	-- Hack alert. Threading state to here was too much bother.
	topdir <- getEnvDefault "TOPDIR" ""
	changeWorkingDirectory $ topdir ++ "/" ++ t

tmpdir :: String
tmpdir = ".t"

mainrepodir :: String
mainrepodir = tmpdir ++ "/repo"

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

changecontent :: FilePath -> IO ()
changecontent f = writeFile f $ changedcontent f

changedcontent :: FilePath -> String
changedcontent f = (content f) ++ " (modified)"
