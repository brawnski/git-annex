{- Checks system configuration and generates SysConfig.hs. -}

import System.Directory
import Data.List

import TestConfig

tests :: [TestCase]
tests = [
	  TestCase "version" $ getVersion
	, testCp "cp_a" "-a"
	, testCp "cp_p" "-p"
	, testCp "cp_reflink_auto" "--reflink=auto"
	, TestCase "uuid generator" $ selectCmd "uuid" ["uuid", "uuidgen"]
	, TestCase "xargs -0" $ requireCmd "xargs_0" "xargs -0 </dev/null"
	, TestCase "rsync" $ requireCmd "rsync" "rsync --version >/dev/null"
	, TestCase "unicode FilePath support" $ unicodeFilePath
	] ++ shaTestCases [1, 256, 512, 224, 384]

shaTestCases :: [Int] -> [TestCase]
shaTestCases l = map make l
	where
		make n = 
			let cmd = "sha" ++ show n ++ "sum"
			in TestCase cmd $ requireCmd cmd (cmd ++ " </dev/null")

tmpDir :: String
tmpDir = "tmp"

testFile :: String
testFile = tmpDir ++ "/testfile"

testCp :: ConfigKey -> String -> TestCase
testCp k option = TestCase cmd $ testCmd k run
	where
		cmd = "cp " ++ option
		run = cmd ++ " " ++ testFile ++ " " ++ testFile ++ ".new"

{- Checks if FilePaths contain decoded unicode, or not. The testdata
 - directory contains a "unicode-test-ü" file; try to find the file,
 - and see if the "ü" is encoded correctly.
 -
 - Note that the file is shipped with git-annex, rather than created,
 - to avoid other potential unicode issues.
 -}
unicodeFilePath :: Test
unicodeFilePath = do
	fs <- getDirectoryContents "testdata"
	let file = head $ filter (isInfixOf "unicode-test") fs
	return $ Config "unicodefilepath" (BoolConfig $ isInfixOf "ü" file)

{- Pulls package version out of the changelog. -}
getVersion :: Test
getVersion = do
	changelog <- readFile "debian/changelog"
	let verline = head $ lines changelog
	let version = middle (words verline !! 1)
	return $ Config "packageversion" (StringConfig version)
	where
		middle s = drop 1 $ take (length s - 1) s

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
