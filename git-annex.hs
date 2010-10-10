{- git-annex main program
 - -}

import LocationLog
import GitRepo
import Backend
import Annex

-- When adding a new backend, import it here and add it to the backends list.
import qualified BackendFile
import qualified BackendChecksum
import qualified BackendUrl
backends = [BackendFile.backend, BackendChecksum.backend, BackendUrl.backend]

main = do
	repo <- currentRepo
	gitPrep repo

	l <- readLog "demo.log"
	writeLog "demo2.log" $ compactLog l
