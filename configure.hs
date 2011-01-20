{- Checks system configuration and generates SysConfig.hs. -}

import System.Directory

import TestConfig

tests :: [TestCase]
tests = [
	  testCp "cp_a" "-a"
	, testCp "cp_p" "-p"
	, testCp "cp_reflink_auto" "--reflink=auto"
	, TestCase "uuid generator" $ selectCmd "uuid" ["uuid", "uuidgen"]
	, TestCase "xargs -0" $ requireCmd "xargs_0" "xargs -0 </dev/null"
	, TestCase "rsync" $ requireCmd "rsync" "rsync --version >/dev/null"
	]

tmpDir :: String
tmpDir = "tmp"

testFile :: String
testFile = tmpDir ++ "/testfile"

testCp :: ConfigKey -> String -> TestCase
testCp k option = TestCase cmd $ testCmd k run
	where
		cmd = "cp " ++ option
		run = cmd ++ " " ++ testFile ++ " " ++ testFile ++ ".new"

setup :: IO ()
setup = do
	createDirectoryIfMissing True tmpDir
	writeFile testFile "test file contents"

cleanup :: IO ()
cleanup = do
	removeDirectoryRecursive tmpDir

main :: IO ()
main = do
	setup
	config <- runTests tests
	writeSysConfig config
	cleanup
