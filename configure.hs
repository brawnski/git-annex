{- Checks system configuration and generates SysConfig.hs. -}

import System.Directory
import Data.List

import TestConfig

tests :: [TestCase]
tests =
	[ TestCase "version" $ getVersion
	, testCp "cp_a" "-a"
	, testCp "cp_p" "-p"
	, testCp "cp_reflink_auto" "--reflink=auto"
	, TestCase "uuid generator" $ selectCmd "uuid" ["uuid", "uuidgen"] ""
	, TestCase "xargs -0" $ requireCmd "xargs_0" "xargs -0 </dev/null"
	, TestCase "rsync" $ requireCmd "rsync" "rsync --version >/dev/null"
	, TestCase "curl" $ testCmd "curl" "curl --version >/dev/null"
	, TestCase "bup" $ testCmd "bup" "bup --version >/dev/null"
	, TestCase "gpg" $ testCmd "gpg" "gpg --version >/dev/null"
	, TestCase "unicode FilePath support" $ unicodeFilePath
	] ++ shaTestCases [1, 256, 512, 224, 384]

shaTestCases :: [Int] -> [TestCase]
shaTestCases l = map make l
	where make n =
		let
			cmds = map (\x -> "sha" ++ show n ++ x) ["", "sum"]
			key = "sha" ++ show n
		in TestCase key $ maybeSelectCmd key cmds "</dev/null"

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
	changelog <- readFile "CHANGELOG"
	let verline = head $ lines changelog
	let version = middle (words verline !! 1)

	-- Replace Version field in cabal file, so I don't have to maintain
	-- the version there too.
	cabal <- readFile cabalfile
	writeFile tmpcabalfile $ unlines $ map (setversion version) $ lines cabal
	renameFile tmpcabalfile cabalfile

	return $ Config "packageversion" (StringConfig version)
	where
		middle s = drop 1 $ take (length s - 1) s
		cabalfile = "git-annex.cabal"
		tmpcabalfile = cabalfile++".tmp"
		setversion version s
			| "Version:" `isPrefixOf` s = "Version: " ++ version
			| otherwise = s

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
