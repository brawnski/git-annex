{- git-annex backend list
 - -}

module BackendList (
	supportedBackends,
	parseBackendList,
	lookupBackendName
) where

import BackendTypes

-- When adding a new backend, import it here and add it to the list.
import qualified Backend.WORM
import qualified Backend.SHA1
import qualified Backend.URL
supportedBackends = 
	[ Backend.WORM.backend
	, Backend.SHA1.backend
	, Backend.URL.backend
	]

{- Parses a string with a list of backend names into
 - a list of Backend objects. If the list is empty,
 - defaults to supportedBackends. -}
parseBackendList :: String -> [Backend]
parseBackendList s = 
	if (length s == 0)
		then supportedBackends
		else map (lookupBackendName) $ words s

{- Looks up a supported backend by name. -}
lookupBackendName :: String -> Backend
lookupBackendName s =
	if ((length matches) /= 1)
		then error $ "unknown backend " ++ s
		else matches !! 0
	where matches = filter (\b -> s == name b) supportedBackends
