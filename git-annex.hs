{- git-annex main program
 - -}

import System.IO
import System.Environment
import Control.Exception
import CmdLine
import Annex

main = do
	args <- getArgs
	(mode, files) <- argvToMode args
	
	state <- startAnnex

	tryRun 0 $ map (\f -> dispatch state mode f) files

{- Tries to run a series of actions, not stopping if some error out,
 - and propigating an overall error status at the end. -}
tryRun errflag [] = do
	if (errflag > 0)
		then error "unsuccessful"
		else return ()
tryRun errflag (a:as) = do
	result <- try (a)::IO (Either SomeException ())
	case (result) of
		Left err -> do
			showErr err
			tryRun 1 as
		Right _ -> tryRun errflag as

{- Exception pretty-printing. -}
showErr :: SomeException -> IO ()
showErr e = do
	let err = show e
	hPutStrLn stderr $ "git-annex: " ++ err
	return ()
