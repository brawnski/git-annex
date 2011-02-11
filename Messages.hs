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
import qualified Codec.Binary.UTF8.String as UTF8

import Types
import qualified Annex
import qualified SysConfig

verbose :: Annex () -> Annex ()
verbose a = do
	q <- Annex.getState Annex.quiet
	unless q a

showSideAction :: String -> Annex ()
showSideAction s = verbose $ liftIO $ putStrLn $ "(" ++ s ++ ")"

showStart :: String -> String -> Annex ()
showStart command file = verbose $ do
	liftIO $ putStr $ command ++ " " ++ filePathToString file ++ " "
	liftIO $ hFlush stdout

showNote :: String -> Annex ()
showNote s = verbose $ do
	liftIO $ putStr $ "(" ++ s ++ ") "
	liftIO $ hFlush stdout

showProgress :: Annex ()
showProgress = verbose $ liftIO $ putStr "\n"

showLongNote :: String -> Annex ()
showLongNote s = verbose $ liftIO $ putStr $ "\n" ++ indent s

showEndOk :: Annex ()
showEndOk = verbose $ liftIO $ putStrLn "ok"

showEndFail :: Annex ()
showEndFail = verbose $ liftIO $ putStrLn "\nfailed"

showErr :: (Show a) => a -> Annex ()
showErr e = warning $ "git-annex: " ++ show e

warning :: String -> Annex ()
warning w = do
	verbose $ liftIO $ putStr "\n"
	liftIO $ hFlush stdout
	liftIO $ hPutStrLn stderr $ indent w

indent :: String -> String
indent s = join "\n" $ map (\l -> "  " ++ l) $ lines s

{- Prepares a filename for display. This is needed because on many
 - platforms (eg, unix), FilePaths are internally stored in
 - non-decoded form. -}
filePathToString :: FilePath -> String
filePathToString = if SysConfig.unicodefilepath then id else UTF8.decodeString
