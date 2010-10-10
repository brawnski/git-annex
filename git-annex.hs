{- git-annex main program
 - -}

import LocationLog
import GitRepo
import Backend

-- When adding a new backend, import it here and add it to the backends list.
import qualified BackendFile
import qualified BackendUrl
backends = [BackendFile.backend, BackendUrl.backend]

main = do
	repo <- repoTop
	gitPrep repo

	l <- readLog "demo.log"
	writeLog "demo2.log" $ compactLog l
