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

	tryRun 0 0 $ map (\f -> dispatch state mode f) files

{- Tries to run a series of actions, not stopping if some error out,
 - and propigating an overall error status at the end. -}
tryRun errnum oknum [] = do
	if (errnum > 0)
		then error $ (show errnum) ++ " failed ; " ++ show (oknum) ++ " succeeded"
		else return ()
tryRun errnum oknum (a:as) = do
	result <- try (a)::IO (Either SomeException ())
	case (result) of
		Left err -> do
			showErr err
			tryRun (errnum + 1) oknum as
		Right _ -> tryRun errnum (oknum + 1) as

{- Exception pretty-printing. -}
showErr :: SomeException -> IO ()
showErr e = do
	let err = show e
	hPutStrLn stderr $ "git-annex: " ++ err
	return ()
