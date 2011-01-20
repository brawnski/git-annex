{- Tests the system and generates SysConfig.hs. -}

module TestConfig where

import System.IO
import System.Cmd
import System.Exit

type ConfigKey = String
data ConfigValue = BoolConfig Bool | StringConfig String
data Config = Config ConfigKey ConfigValue

type Test = IO Config
type TestName = String
data TestCase = TestCase TestName Test

instance Show ConfigValue where
	show (BoolConfig b) = show b
	show (StringConfig s) = show s

instance Show Config where
        show (Config key value) = unlines
		[ key ++ " :: " ++ valuetype value
		, key ++ " = " ++ show value
		]
		where
			valuetype (BoolConfig _) = "Bool"
			valuetype (StringConfig _) = "String"

writeSysConfig :: [Config] -> IO ()
writeSysConfig config = writeFile "SysConfig.hs" body
	where
		body = unlines $ header ++ map show config ++ footer
		header = [
			  "{- Automatically generated. -}"
			, "module SysConfig where"
			, ""
			]
		footer = []

runTests :: [TestCase] -> IO [Config]
runTests [] = return []
runTests ((TestCase tname t):ts) = do
	testStart tname
	c <- t
	testEnd c
	rest <- runTests ts
	return $ c:rest

{- Tests that a command is available, aborting if not. -}
requireCmd :: ConfigKey -> String -> Test
requireCmd k cmdline = do
	ret <- testCmd k cmdline
	handle ret
	where
		handle r@(Config _ (BoolConfig True)) = return r
		handle r = do
			testEnd r
			error $ "** the " ++ c ++ " command is required"
		c = (words cmdline) !! 0

{- Checks if a command is available by running a command line. -}
testCmd :: ConfigKey -> String -> Test
testCmd k cmdline = do
	ret <- system $ quiet cmdline
	return $ Config k (BoolConfig $ ret == ExitSuccess)

{- Ensures that one of a set of commands is available by running each in
 - turn. The Config is set to the first one found. -}
selectCmd :: ConfigKey -> [String] -> Test
selectCmd k cmds = search cmds
	where
		search [] = do
			testEnd $ Config k (BoolConfig False)
			error $ "* need one of these commands, but none are available: " ++ show cmds
		search (c:cs) = do
			ret <- system $ quiet c
			if (ret == ExitSuccess)
				then return $ Config k (StringConfig c)
				else search cs

quiet :: String -> String
quiet s = s ++ " >/dev/null 2>&1"

testStart :: TestName -> IO ()
testStart s = do
	putStr $ "  checking " ++ s ++ "..."
	hFlush stdout

testEnd :: Config -> IO ()
testEnd (Config _ (BoolConfig True)) = putStrLn $ " yes"
testEnd (Config _ (BoolConfig False)) = putStrLn $ " no"
testEnd (Config _ (StringConfig s)) = putStrLn $ " " ++ s
