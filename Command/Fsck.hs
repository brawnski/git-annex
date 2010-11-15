{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Fsck where

import qualified Data.Map as M

import Command
import Types
import Core
import Messages
import qualified Command.FsckFile
import qualified Command.Unused

seek :: [SubCmdSeek]
seek = [withNothing Command.Unused.start, withAll withFilesInGit Command.FsckFile.start]
