{- git-annex main program -}

import Control.Exception
import System.IO
import System.Environment

import qualified Annex
import Types
import Core
import Commands
import qualified GitRepo as Git
import BackendList

main = do
	args <- getArgs
	gitrepo <- Git.repoFromCwd
	state <- Annex.new gitrepo allBackends
	(flags, actions) <- parseCmd args state
	tryRun state $ [startup flags] ++ actions ++ [shutdown]

{- Runs a list of Annex actions. Catches exceptions, not stopping
 - if some error out, and propigates an overall error status at the end.
 -
 - This runs in the IO monad, not in the Annex monad. It seems that
 - exceptions can only be caught in the IO monad, not in a stacked monad;
 - or more likely I missed an easy way to do it. So, I have to laboriously
 - thread AnnexState through this function.
 -}
tryRun :: AnnexState -> [Annex ()] -> IO ()
tryRun state actions = tryRun' state 0 actions
tryRun' state errnum (a:as) = do
	result <- try
		(Annex.run state a)::IO (Either SomeException ((), AnnexState))
	case (result) of
		Left err -> do
			showErr err
			tryRun' state (errnum + 1) as
		Right (_,state') -> tryRun' state' errnum as
tryRun' state errnum [] = do
	if (errnum > 0)
		then error $ (show errnum) ++ " failed"
		else return ()

{- Exception pretty-printing. -}
showErr e = do
	hPutStrLn stderr $ "git-annex: " ++ (show e)
	return ()
