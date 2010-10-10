{- git-annex main program
 - -}

import LocationLog
import GitRepo

main = do
	repo <- repoTop
	gitPrep repo
	l <- readLog "demo.log"
	writeLog "demo2.log" $ compactLog l
