{- git-annex main program
 - -}

import LocationLog

main = do
	l <- readLog "demo.log"
	putStrLn "hi"
