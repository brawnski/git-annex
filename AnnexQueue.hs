{- git-annex command queue
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module AnnexQueue (
	add,
	flush,
	flushWhenFull
) where

import Control.Monad.State (liftIO)
import Control.Monad (when, unless)

import Annex
import Messages
import qualified GitQueue
import Utility

{- Adds a git command to the queue, possibly running previously queued
 - actions if enough have accumulated. -}
add :: String -> [CommandParam] -> FilePath -> Annex ()
add command params file = do
	q <- getState repoqueue
	store $ GitQueue.add q command params file

{- Runs the queue if it is full. Should be called periodically. -}
flushWhenFull :: Annex ()
flushWhenFull = do
	q <- getState repoqueue
	when (GitQueue.full q) $ flush False

{- Runs (and empties) the queue. -}
flush :: Bool -> Annex ()
flush silent = do
	q <- getState repoqueue
	unless (0 == GitQueue.size q) $ do
		unless silent $
			showSideAction "Recording state in git..."
		g <- gitRepo
		q' <- liftIO $ GitQueue.flush g q
		store q'

store :: GitQueue.Queue -> Annex ()
store q = changeState $ \s -> s { repoqueue = q }
