{- git-annex test suite
 -
 - Copyright 2010,2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

import Test.HUnit
import Test.HUnit.Tools
import System.Directory
import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.Files
import IO (bracket_, bracket)
import Control.Monad (unless, when, filterM)
import Data.List
import System.IO.Error
import System.Posix.Env
import qualified Control.Exception.Extensible as E
import Control.Exception (throw)
import Control.Monad.State (liftIO)
import Maybe
import qualified Data.Map as M
import System.Path (recurseDir)
import System.IO.HVFS (SystemFS(..))

import qualified Annex
import qualified BackendList
import qualified Backend
import qualified GitRepo as Git
import qualified Locations
import qualified Utility
import qualified BackendTypes
import qualified Types
import qualified GitAnnex
import qualified LocationLog
import qualified UUID
import qualified Trust
import qualified Remotes
import qualified Content
import qualified Backend.SHA1
import qualified Backend.WORM
import qualified Command.DropUnused

main :: IO ()
main = do
	prepare
	r <- runVerboseTests $ TestList [quickcheck, blackbox]
	cleanup tmpdir
	propigate r

propigate :: (Counts, Int) -> IO ()
propigate (Counts { errors = e }, _)
	| e > 0 = error "failed"
	| otherwise = return ()

quickcheck :: Test
quickcheck = TestLabel "quickcheck" $ TestList
	[ qctest "prop_idempotent_deencode" Git.prop_idempotent_deencode
	, qctest "prop_idempotent_fileKey" Locations.prop_idempotent_fileKey
	, qctest "prop_idempotent_key_read_show" BackendTypes.prop_idempotent_key_read_show
	, qctest "prop_idempotent_shellEscape" Utility.prop_idempotent_shellEscape
	, qctest "prop_idempotent_shellEscape_multiword" Utility.prop_idempotent_shellEscape_multiword
	, qctest "prop_parentDir_basics" Utility.prop_parentDir_basics
	, qctest "prop_relPathDirToDir_basics" Utility.prop_relPathDirToDir_basics
	]

blackbox :: Test
blackbox = TestLabel "blackbox" $ TestList
	-- test order matters, later tests may rely on state from earlier
	[ test_init
	, test_add
	, test_setkey
	, test_unannex
	, test_drop
	, test_get
	, test_move
	, test_copy
	, test_lock
	, test_edit
	, test_fix
	, test_trust
	, test_fsck
	, test_migrate
	, test_unused
	]

test_init :: Test
test_init = "git-annex init" ~: TestCase $ innewrepo $ do
	git_annex "init" ["-q", reponame] @? "init failed"
	e <- doesFileExist annexlog
	e @? (annexlog ++ " not created")
	c <- readFile annexlog
	reponame `isInfixOf` c @? annexlog ++ " does not contain repo name"
	where
		annexlog = ".git-annex/uuid.log"
		reponame = "test repo"

test_add :: Test
test_add = "git-annex add" ~: TestList [basic, sha1dup]
	where
		-- this test case runs in the main repo, to set up a basic
		-- annexed file that later tests will use
		basic = TestCase $ inmainrepo $ do
			writeFile annexedfile $ content annexedfile
			git_annex "add" ["-q", annexedfile] @? "add failed"
			annexed_present annexedfile
			writeFile ingitfile $ content ingitfile
			Utility.boolSystem "git" ["add", ingitfile] @? "git add failed"
			Utility.boolSystem "git" ["commit", "-q", "-a", "-m", "commit"] @? "git commit failed"
			git_annex "add" ["-q", ingitfile] @? "add ingitfile should be no-op"
			unannexed ingitfile
		sha1dup = TestCase $ intmpclonerepo $ do
			writeFile sha1annexedfile $ content sha1annexedfile
			git_annex "add" ["-q", sha1annexedfile, "--backend=SHA1"] @? "add with SHA1 failed"
			annexed_present sha1annexedfile
			writeFile sha1annexedfiledup $ content sha1annexedfiledup
			git_annex "add" ["-q", sha1annexedfiledup, "--backend=SHA1"] @? "add of second file with same SHA1 failed"
			annexed_present sha1annexedfiledup
			annexed_present sha1annexedfile

test_setkey :: Test
test_setkey = "git-annex setkey/fromkey" ~: TestCase $ inmainrepo $ do
	writeFile tmp $ content sha1annexedfile
	r <- annexeval $ BackendTypes.getKey Backend.SHA1.backend tmp
	let sha1 = BackendTypes.keyName $ fromJust r
	git_annex "setkey" ["-q", "--backend", "SHA1", "--key", sha1, tmp] @? "setkey failed"
	git_annex "fromkey" ["-q", "--backend", "SHA1", "--key", sha1, sha1annexedfile] @? "fromkey failed"
	Utility.boolSystem "git" ["commit", "-q", "-a", "-m", "commit"] @? "git commit failed"
	annexed_present sha1annexedfile
	where
		tmp = "tmpfile"

test_unannex :: Test
test_unannex = "git-annex unannex" ~: TestList [nocopy, withcopy]
	where
		nocopy = "no content" ~: intmpclonerepo $ do
			annexed_notpresent annexedfile
			git_annex "unannex" ["-q", annexedfile] @? "unannex failed with no copy"
			annexed_notpresent annexedfile
		withcopy = "with content" ~: intmpclonerepo $ do
			git_annex "get" ["-q", annexedfile] @? "get failed"
			Utility.boolSystem "git" ["commit", "-q", "-a", "-m", "state changed"]
				@? "git commit of state failed"
			annexed_present annexedfile
			git_annex "unannex" ["-q", annexedfile, sha1annexedfile] @? "unannex failed"
			unannexed annexedfile
			git_annex "unannex" ["-q", annexedfile] @? "unannex failed on non-annexed file"
			unannexed annexedfile
			git_annex "unannex" ["-q", ingitfile] @? "unannex ingitfile should be no-op"
			unannexed ingitfile

test_drop :: Test
test_drop = "git-annex drop" ~: TestList [noremote, withremote, untrustedremote]
	where
		noremote = "no remotes" ~: TestCase $ intmpclonerepo $ do
			git_annex "get" ["-q", annexedfile] @? "get failed"
			Utility.boolSystem "git" ["commit", "-q", "-a", "-m", "state changed"]
				@? "git commit of state failed"
			Utility.boolSystem "git" ["remote", "rm", "origin"]
				@? "git remote rm origin failed"
			r <- git_annex "drop" ["-q", annexedfile]
			not r @? "drop wrongly succeeded with no known copy of file"
			annexed_present annexedfile
			git_annex "drop" ["-q", "--force", annexedfile] @? "drop --force failed"
			annexed_notpresent annexedfile
			git_annex "drop" ["-q", annexedfile] @? "drop of dropped file failed"
			git_annex "drop" ["-q", ingitfile] @? "drop ingitfile should be no-op"
			unannexed ingitfile
		withremote = "with remote" ~: TestCase $ intmpclonerepo $ do
			git_annex "get" ["-q", annexedfile] @? "get failed"
			annexed_present annexedfile
			git_annex "drop" ["-q", annexedfile] @? "drop failed though origin has copy"
			annexed_notpresent annexedfile
			inmainrepo $ annexed_present annexedfile
		untrustedremote = "untrusted remote" ~: TestCase $ intmpclonerepo $ do
			git_annex "untrust" ["-q", "origin"] @? "untrust of origin failed"
			git_annex "get" ["-q", annexedfile] @? "get failed"
			annexed_present annexedfile
			r <- git_annex "drop" ["-q", annexedfile]
			not r @? "drop wrongly suceeded with only an untrusted copy of the file"
			annexed_present annexedfile
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
	-- regression test: unlock of not present file should skip it
	annexed_notpresent annexedfile
	r <- git_annex "unlock" ["-q", annexedfile]
	not r @? "unlock failed to fail with not present file"
	annexed_notpresent annexedfile

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
	r' <- git_annex "drop" ["-q", annexedfile]
	not r' @? "drop wrongly succeeded with no known copy of modified file"

test_edit :: Test
test_edit = "git-annex edit/commit" ~: TestList [t False, t True]
	where t precommit = TestCase $ intmpclonerepo $ do
		git_annex "get" ["-q", annexedfile] @? "get of file failed"
		annexed_present annexedfile
		git_annex "edit" ["-q", annexedfile] @? "edit failed"
		unannexed annexedfile
		changecontent annexedfile
		if precommit
			then do
				-- pre-commit depends on the file being
				-- staged, normally git commit does this
				Utility.boolSystem "git" ["add", annexedfile]
					@? "git add of edited file failed"
				git_annex "pre-commit" ["-q"]
					@? "pre-commit failed"
			else do
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

test_trust :: Test
test_trust = "git-annex trust/untrust/semitrust" ~: intmpclonerepo $ do
	git_annex "trust" ["-q", repo] @? "trust failed"
	trustcheck Trust.Trusted "trusted 1"
	git_annex "trust" ["-q", repo] @? "trust of trusted failed"
	trustcheck Trust.Trusted "trusted 2"
	git_annex "untrust" ["-q", repo] @? "untrust failed"
	trustcheck Trust.UnTrusted "untrusted 1"
	git_annex "untrust" ["-q", repo] @? "untrust of untrusted failed"
	trustcheck Trust.UnTrusted "untrusted 2"
	git_annex "semitrust" ["-q", repo] @? "semitrust failed"
	trustcheck Trust.SemiTrusted "semitrusted 1"
	git_annex "semitrust" ["-q", repo] @? "semitrust of semitrusted failed"
	trustcheck Trust.SemiTrusted "semitrusted 2"
	where
		trustcheck expected msg = do
			present <- annexeval $ do
				Remotes.readConfigs
				l <- Trust.trustGet expected
				r <- Remotes.byName repo
				u <- UUID.getUUID r
				return $ u `elem` l
			assertBool msg present
		repo = "origin"

test_fsck :: Test
test_fsck = "git-annex fsck" ~: TestList [basicfsck, withlocaluntrusted, withremoteuntrusted]
	where
		basicfsck = TestCase $ intmpclonerepo $ do
			git_annex "fsck" ["-q"] @? "fsck failed"
			Utility.boolSystem "git" ["config", "annex.numcopies", "2"] @? "git config failed"
			fsck_should_fail "numcopies unsatisfied"
			Utility.boolSystem "git" ["config", "annex.numcopies", "1"] @? "git config failed"
			corrupt annexedfile
			corrupt sha1annexedfile
		withlocaluntrusted = TestCase $ intmpclonerepo $ do
			git_annex "get" ["-q", annexedfile] @? "get failed"
			git_annex "untrust" ["-q", "origin"] @? "untrust of origin repo failed"
			git_annex "untrust" ["-q", "."] @? "untrust of current repo failed"
			fsck_should_fail "content only available in untrusted (current) repository"
			git_annex "trust" ["-q", "."] @? "trust of current repo failed"
			git_annex "fsck" ["-q", annexedfile] @? "fsck failed on file present in trusted repo"
		withremoteuntrusted = TestCase $ intmpclonerepo $ do
			Utility.boolSystem "git" ["config", "annex.numcopies", "2"] @? "git config failed"
			git_annex "get" ["-q", annexedfile] @? "get failed"
			git_annex "get" ["-q", sha1annexedfile] @? "get failed"
			git_annex "fsck" ["-q"] @? "fsck failed with numcopies=2 and 2 copies"
			git_annex "untrust" ["-q", "origin"] @? "untrust of origin failed"
			fsck_should_fail "content not replicated to enough non-untrusted repositories"
		corrupt f = do
			git_annex "get" ["-q", f] @? "get of file failed"
			Content.allowWrite f
			writeFile f (changedcontent f)
			r <- git_annex "fsck" ["-q"]
			not r @? "fsck failed to fail with corrupted file content"
			git_annex "fsck" ["-q"] @? "fsck unexpectedly failed again; previous one did not fix problem"
		fsck_should_fail m = do
			r <- git_annex "fsck" ["-q"]
			not r @? "fsck failed to fail with " ++ m

test_migrate :: Test
test_migrate = "git-annex migrate" ~: TestList [t False, t True]
	where t usegitattributes = TestCase $ intmpclonerepo $ do
		annexed_notpresent annexedfile
		annexed_notpresent sha1annexedfile
		git_annex "migrate" ["-q", annexedfile] @? "migrate of not present failed"
		git_annex "migrate" ["-q", sha1annexedfile] @? "migrate of not present failed"
		git_annex "get" ["-q", annexedfile] @? "get of file failed"
		git_annex "get" ["-q", sha1annexedfile] @? "get of file failed"
		annexed_present annexedfile
		annexed_present sha1annexedfile
		if usegitattributes
			then do
				writeFile ".gitattributes" $ "* annex.backend=SHA1"
				git_annex "migrate" ["-q", sha1annexedfile]
					@? "migrate sha1annexedfile failed"
				git_annex "migrate" ["-q", annexedfile]
					@? "migrate annexedfile failed"
			else do
				git_annex "migrate" ["-q", sha1annexedfile, "--backend", "SHA1"]
					@? "migrate sha1annexedfile failed"
				git_annex "migrate" ["-q", annexedfile, "--backend", "SHA1"]
					@? "migrate annexedfile failed"
		annexed_present annexedfile
		annexed_present sha1annexedfile
		checkbackend annexedfile Backend.SHA1.backend
		checkbackend sha1annexedfile Backend.SHA1.backend

		-- check that reversing a migration works
		writeFile ".gitattributes" $ "* annex.backend=WORM"
		git_annex "migrate" ["-q", sha1annexedfile]
			@? "migrate sha1annexedfile failed"
		git_annex "migrate" ["-q", annexedfile]
			@? "migrate annexedfile failed"
		annexed_present annexedfile
		annexed_present sha1annexedfile
		checkbackend annexedfile Backend.WORM.backend
		checkbackend sha1annexedfile Backend.WORM.backend
		
		where
			checkbackend file expected = do
				r <- annexeval $ Backend.lookupFile file
				let b = snd $ fromJust r
				assertEqual ("backend for " ++ file) expected b

test_unused :: Test
test_unused = "git-annex unused/dropunused" ~: intmpclonerepo $ do
	-- keys have to be looked up before files are removed
	annexedfilekey <- annexeval $ findkey annexedfile
	sha1annexedfilekey <- annexeval $ findkey sha1annexedfile
	git_annex "get" ["-q", annexedfile] @? "get of file failed"
	git_annex "get" ["-q", sha1annexedfile] @? "get of file failed"
	checkunused []
	Utility.boolSystem "git" ["rm", "-q", annexedfile] @? "git rm failed"
	checkunused [annexedfilekey]
	Utility.boolSystem "git" ["rm", "-q", sha1annexedfile] @? "git rm failed"
	checkunused [annexedfilekey, sha1annexedfilekey]

	-- good opportunity to test dropkey also
	git_annex "dropkey" ["-q", "--force", BackendTypes.keyName annexedfilekey]
		@? "dropkey failed"
	checkunused [sha1annexedfilekey]

	git_annex "dropunused" ["-q", "1", "2"] @? "dropunused failed"
	checkunused []
	git_annex "dropunused" ["-q", "10", "501"] @? "dropunused failed on bogus numbers"

	where
		checkunused expectedkeys = do
			git_annex "unused" ["-q"] @? "unused failed"
			unusedmap <- annexeval $ Command.DropUnused.readUnusedLog
			let unusedkeys = M.elems unusedmap
			assertEqual "unused keys differ"
				(sort expectedkeys) (sort unusedkeys)
		findkey f = do
			r <- Backend.lookupFile f
			return $ fst $ fromJust r

-- This is equivilant to running git-annex, but it's all run in-process
-- so test coverage collection works.
git_annex :: String -> [String] -> IO Bool
git_annex command params = do
	-- catch all errors, including normally fatal errors
	r <- E.try (run)::IO (Either E.SomeException ())
	case r of
		Right _ -> return True
		Left _ -> return False
	where
		run = GitAnnex.run (command:params)

-- Runs an action in the current annex. Note that shutdown actions
-- are not run; this should only be used for actions that query state.
annexeval :: Types.Annex a -> IO a
annexeval a = do
	g <- Git.repoFromCwd
	g' <- Git.configRead g
	s <- Annex.new g' BackendList.allBackends
	Annex.eval s a

innewrepo :: Assertion -> Assertion
innewrepo a = withgitrepo $ \r -> indir r a

inmainrepo :: Assertion -> Assertion
inmainrepo a = indir mainrepodir a

intmpclonerepo :: Assertion -> Assertion
intmpclonerepo a = withtmpclonerepo $ \r -> indir r a

withtmpclonerepo :: (FilePath -> Assertion) -> Assertion
withtmpclonerepo = bracket (clonerepo mainrepodir tmprepodir) cleanup

withgitrepo :: (FilePath -> Assertion) -> Assertion
withgitrepo = bracket (setuprepo mainrepodir) return

indir :: FilePath -> Assertion -> Assertion
indir dir a = do
	cwd <- getCurrentDirectory
	-- Assertion failures throw non-IO errors; catch
	-- any type of error and change back to cwd before
	-- rethrowing.
	r <- bracket_ (changeToTmpDir dir)
		(\_ -> changeWorkingDirectory cwd)
		(E.try (a)::IO (Either E.SomeException ()))
	case r of
		Right () -> return ()
		Left e -> throw e

setuprepo :: FilePath -> IO FilePath
setuprepo dir = do
	cleanup dir
	ensuretmpdir
	Utility.boolSystem "git" ["init", "-q", dir] @? "git init failed"
	indir dir $ do
		Utility.boolSystem "git" ["config", "user.name", "Test User"] @? "git config failed"
		Utility.boolSystem "git" ["config", "user.email", "test@example.com"] @? "git config failed"
	return dir

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
		-- removed via directory permissions; undo
		recurseDir SystemFS dir >>=
			filterM doesDirectoryExist >>=
			mapM_ Content.allowWrite
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

checklocationlog :: FilePath -> Bool -> Assertion
checklocationlog f expected = do
	thisuuid <- annexeval $ do
		g <- Annex.gitRepo
		UUID.getUUID g
	r <- annexeval $ Backend.lookupFile f
	case r of
		Just (k, _) -> do
			uuids <- annexeval $ do
				g <- Annex.gitRepo
				liftIO $ LocationLog.keyLocations g k
			assertEqual ("bad content in location log for " ++ f ++ " key " ++ (show k) ++ " uuid " ++ thisuuid)
				expected (thisuuid `elem` uuids)

			-- Location log files should always be checked
			-- into git, and any modifications staged for
			-- commit. This is a regression test, as some
			-- commands forgot to.
			(fs, ufs) <- annexeval $ do
				g <- Annex.gitRepo
				let lf = LocationLog.logFile g k
				fs <- liftIO $ Git.inRepo g [lf]
				ufs <- liftIO $ Git.changedUnstagedFiles g [lf]
				return (fs, ufs)
			when (null fs) $
				assertFailure $ f ++ " logfile not added to git repo"
			when (not $ null ufs) $
				assertFailure $ f ++ " logfile changes not staged"
		_ -> assertFailure $ f ++ " failed to look up key"

inlocationlog :: FilePath -> Assertion
inlocationlog f = checklocationlog f True

notinlocationlog :: FilePath -> Assertion
notinlocationlog f = checklocationlog f False

runchecks :: [FilePath -> Assertion] -> FilePath -> Assertion
runchecks [] _ = return ()
runchecks (a:as) f = do
	a f
	runchecks as f

annexed_notpresent :: FilePath -> Assertion
annexed_notpresent = runchecks
	[checklink, checkdangling, checkunwritable, notinlocationlog]

annexed_present :: FilePath -> Assertion
annexed_present = runchecks
	[checklink, checkcontent, checkunwritable, inlocationlog]

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

sha1annexedfile :: String
sha1annexedfile = "sha1foo"

sha1annexedfiledup :: String
sha1annexedfiledup = "sha1foodup"

ingitfile :: String
ingitfile = "bar"

content :: FilePath -> String		
content f
	| f == annexedfile = "annexed file content"
	| f == ingitfile = "normal file content"
	| f == sha1annexedfile ="sha1 annexed file content"
	| f == sha1annexedfiledup = content sha1annexedfile
	| otherwise = "unknown file " ++ f

changecontent :: FilePath -> IO ()
changecontent f = writeFile f $ changedcontent f

changedcontent :: FilePath -> String
changedcontent f = (content f) ++ " (modified)"
