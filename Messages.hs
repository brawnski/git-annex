{- git-annex output messages
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Messages where

import Control.Monad.State (liftIO)
import System.IO
import Control.Monad (unless)
import Data.String.Utils

import Types
import qualified Annex

verbose :: Annex () -> Annex ()
verbose a = do
	q <- Annex.flagIsSet "quiet"
	unless q a

showStart :: String -> String -> Annex ()
showStart command file = verbose $ do
	liftIO $ putStr $ command ++ " " ++ file ++ " "
	liftIO $ hFlush stdout

showNote :: String -> Annex ()
showNote s = verbose $ do
	liftIO $ putStr $ "(" ++ s ++ ") "
	liftIO $ hFlush stdout

showProgress :: Annex ()
showProgress = verbose $ liftIO $ putStr "\n"

showLongNote :: String -> Annex ()
showLongNote s = verbose $ do
	liftIO $ putStr $ "\n" ++ indented
	where
		indented = join "\n" $ map (\l -> "  " ++ l) $ lines s 
showEndOk :: Annex ()
showEndOk = verbose $ do
	liftIO $ putStrLn "ok"

showEndFail :: Annex ()
showEndFail = verbose $ do
	liftIO $ putStrLn "\nfailed"

{- Exception pretty-printing. -}
showErr :: (Show a) => a -> Annex ()
showErr e = warning $ show e

warning :: String -> Annex ()
warning s = liftIO $ hPutStrLn stderr $ "git-annex: " ++ s
