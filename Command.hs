{- git-annex command types
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command where

import Types
import Backend

{- A subcommand runs in four stages.
 -
 - 0. The seek stage takes the parameters passed to the subcommand,
 -    looks through the repo to find the ones that are relevant
 -    to that subcommand (ie, new files to add), and generates
 -    a list of start stage actions. -}
type SubCmdSeek = [String] -> Annex [SubCmdStart]
{- 1. The start stage is run before anything is printed about the
  -   subcommand, is passed some input, and can early abort it
  -   if the input does not make sense. It should run quickly and
  -   should not modify Annex state. -}
type SubCmdStart = Annex (Maybe SubCmdPerform)
{- 2. The perform stage is run after a message is printed about the subcommand
 -    being run, and it should be where the bulk of the work happens. -}
type SubCmdPerform = Annex (Maybe SubCmdCleanup)
{- 3. The cleanup stage is run only if the perform stage succeeds, and it
 -    returns the overall success/fail of the subcommand. -}
type SubCmdCleanup = Annex Bool
{- Some helper functions are used to build up SubCmdSeek and SubCmdStart
 - functions. -}
type SubCmdSeekStrings = SubCmdStartString -> SubCmdSeek
type SubCmdStartString = String -> SubCmdStart
type SubCmdSeekBackendFiles = SubCmdStartBackendFile -> SubCmdSeek
type SubCmdStartBackendFile = (FilePath, Maybe Backend) -> SubCmdStart

notAnnexed :: FilePath -> Annex (Maybe a) -> Annex (Maybe a)
notAnnexed file a = do
	r <- Backend.lookupFile file
	case (r) of
		Just _ -> return Nothing
		Nothing -> a

isAnnexed :: FilePath -> ((Key, Backend) -> Annex (Maybe a)) -> Annex (Maybe a)
isAnnexed file a = do
	r <- Backend.lookupFile file
	case (r) of
		Just v -> a v
		Nothing -> return Nothing
