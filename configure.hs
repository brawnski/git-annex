{- Checks system configuration and generates SysConfig.hs. -}

import System.Directory
import Data.List

import TestConfig

tests :: [TestCase]
tests =
	[ TestCase "version" getVersion
	, testCp "cp_a" "-a"
	, testCp "cp_p" "-p"
	, testCp "cp_reflink_auto" "--reflink=auto"
	, TestCase "uuid generator" $ selectCmd "uuid" ["uuid", "uuidgen"] ""
	, TestCase "xargs -0" $ requireCmd "xargs_0" "xargs -0 </dev/null"
	, TestCase "rsync" $ requireCmd "rsync" "rsync --version >/dev/null"
	, TestCase "curl" $ testCmd "curl" "curl --version >/dev/null"
	, TestCase "bup" $ testCmd "bup" "bup --version >/dev/null"
	, TestCase "gpg" $ testCmd "gpg" "gpg --version >/dev/null"
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

{- Pulls package version out of the changelog. -}
getVersion :: Test
getVersion = do
	version <- getVersionString
	return $ Config "packageversion" (StringConfig version)
	
getVersionString :: IO String
getVersionString = do
	changelog <- readFile "CHANGELOG"
	let verline = head $ lines changelog
	return $ middle (words verline !! 1)
	where
		middle s = drop 1 $ take (length s - 1) s

{- Set up cabal file with version. -}
cabalSetup :: IO ()
cabalSetup = do
	version <- getVersionString
	cabal <- readFile cabalfile
	writeFile tmpcabalfile $ unlines $ 
		map (setfield "Version" version) $
		lines cabal
	renameFile tmpcabalfile cabalfile
	where
		cabalfile = "git-annex.cabal"
		tmpcabalfile = cabalfile++".tmp"
		setfield field value s
			| fullfield `isPrefixOf` s = fullfield ++ value
			| otherwise = s
			where
				fullfield = field ++ ": "

setup :: IO ()
setup = do
	createDirectoryIfMissing True tmpDir
	writeFile testFile "test file contents"

cleanup :: IO ()
cleanup = removeDirectoryRecursive tmpDir

main :: IO ()
main = do
	setup
	config <- runTests tests
	writeSysConfig config
	cleanup
	cabalSetup
