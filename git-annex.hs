{- git-annex main program
 - -}

import LocationLog
import GitRepo

main = do
	gitPrep
	l <- readLog "demo.log"
	writeLog "demo2.log" $ l !! 0
