{- git-annex main program
 - -}

import Control.Monad.State
import System.IO
import System.Environment
import Control.Exception
import CmdLine
import Types
import Annex

main = do
	args <- getArgs
	(mode, params) <- argvToMode args
	state <- startAnnex
	tryRun state mode 0 0 params

{- Processes each param in the list by dispatching the handler function
 - for the user-selection operation mode. Catches exceptions, not stopping
 - if some error out, and propigates an overall error status at the end.
 -
 - This runs in the IO monad, not in the Annex monad. It seems that
 - exceptions can only be caught in the IO monad, not in a stacked monad;
 - or more likely I missed an easy way to do it. So, I have to laboriously
 - thread AnnexState through this function.
 -}
tryRun :: AnnexState -> Mode -> Int -> Int -> [String] -> IO ()
tryRun state mode errnum oknum [] = do
	if (errnum > 0)
		then error $ (show errnum) ++ " failed ; " ++ show (oknum) ++ " ok"
		else return ()
tryRun state mode errnum oknum (f:fs) = do
	result <- try (runAnnexState state (dispatch mode f))::IO (Either SomeException ((), AnnexState))
	case (result) of
		Left err -> do
			showErr err
			tryRun state mode (errnum + 1) oknum fs
		Right (_,state') -> tryRun state' mode errnum (oknum + 1) fs

{- Exception pretty-printing. -}
showErr e = do
	hPutStrLn stderr $ "git-annex: " ++ (show e)
	return ()
