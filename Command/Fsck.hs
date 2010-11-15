{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Fsck where

import Command
import qualified Command.FsckFile
import qualified Command.Unused

seek :: [SubCmdSeek]
seek = [withNothing Command.Unused.start, withAll withFilesInGit Command.FsckFile.start]
