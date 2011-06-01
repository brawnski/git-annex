{- git-annex trust
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Trust (
	TrustLevel(..),
	trustLog,
	trustGet,
	trustMap,
	trustMapParse,
	trustSet
) where

import Control.Monad.State
import qualified Data.Map as M

import TrustLevel
import qualified GitRepo as Git
import Types
import UUID
import Locations
import qualified Annex
import qualified Remote
import Utility

{- Filename of trust.log. -}
trustLog :: Annex FilePath
trustLog = do
	g <- Annex.gitRepo
	return $ gitStateDir g ++ "trust.log"

{- Returns a list of UUIDs at the specified trust level. -}
trustGet :: TrustLevel -> Annex [UUID]
trustGet level = do
	m <- trustMap
	return $ M.keys $ M.filter (== level) m

{- Read the trustLog into a map, overriding with any
 - values from forcetrust -}
trustMap :: Annex (M.Map UUID TrustLevel)
trustMap = do
	logfile <- trustLog
	overrides <- Annex.getState Annex.forcetrust >>= mapM findoverride
	s <- liftIO $ catch (readFile logfile) ignoreerror
	return $ M.fromList $ trustMapParse s ++ overrides
	where
                ignoreerror _ = return ""
		findoverride (name, t) = do
			uuid <- Remote.nameToUUID name
			return (uuid, t)

{- Trust map parser. -}
trustMapParse :: String -> [(UUID, TrustLevel)]
trustMapParse s = map pair $ filter (not . null) $ lines s
	where
		pair l
			| length w > 1 = (w !! 0, read (w !! 1) :: TrustLevel)
			-- for back-compat; the trust log used to only
			-- list trusted uuids
			| otherwise = (w !! 0, Trusted)
			where
				w = words l

{- Changes the trust level for a uuid in the trustLog, and commits it. -}
trustSet :: UUID -> TrustLevel -> Annex ()
trustSet uuid level = do
	when (null uuid) $
		error "unknown UUID; cannot modify trust level"
        m <- trustMap
	when (M.lookup uuid m /= Just level) $ do
		let m' = M.insert uuid level m
	        logfile <- trustLog
	        liftIO $ safeWriteFile logfile (serialize m')
		g <- Annex.gitRepo
		liftIO $ Git.run g "add" [File logfile]
		liftIO $ Git.run g "commit"
			[ Params "-q -m"
			, Param "git annex trust change"
			, File logfile
			]
        where
                serialize m = unlines $ map showpair $ M.toList m
		showpair (u, t) = u ++ " " ++ show t
