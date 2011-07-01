{- git-annex remotes
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote (
	Remote,
	uuid,
	name,
	storeKey,
	retrieveKeyFile,
	removeKey,
	hasKey,
	hasKeyCheap,
	keyPossibilities,
	keyPossibilitiesTrusted,
	forceTrust,

	remoteTypes,
	genList,
	byName,
	nameToUUID,
	remotesWithUUID,
	remotesWithoutUUID,
	prettyPrintUUIDs,

	remoteLog,
	readRemoteLog,
	configSet,
	keyValToConfig,
	configToKeyVal,
	
	prop_idempotent_configEscape
) where

import Control.Monad (filterM, liftM2)
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Char

import qualified Branch
import Types
import Types.Remote
import UUID
import qualified Annex
import Config
import Trust
import LocationLog

import qualified Remote.Git
import qualified Remote.S3
import qualified Remote.Bup
import qualified Remote.Directory
import qualified Remote.Rsync
import qualified Remote.Web
import qualified Remote.Hook

remoteTypes :: [RemoteType Annex]
remoteTypes =
	[ Remote.Git.remote
	, Remote.S3.remote
	, Remote.Bup.remote
	, Remote.Directory.remote
	, Remote.Rsync.remote
	, Remote.Web.remote
	, Remote.Hook.remote
	]

{- Builds a list of all available Remotes.
 - Since doing so can be expensive, the list is cached. -}
genList :: Annex [Remote Annex]
genList = do
	rs <- Annex.getState Annex.remotes
	if null rs
		then do
			m <- readRemoteLog
			l <- mapM (process m) remoteTypes
			let rs' = concat l
			Annex.changeState $ \s -> s { Annex.remotes = rs' }
			return rs'
		else return rs
	where
		process m t = 
			enumerate t >>=
			filterM remoteNotIgnored >>=
			mapM (gen m t)
		gen m t r = do
			u <- getUUID r
			generate t r u (M.lookup u m)

{- Looks up a remote by name. (Or by UUID.) -}
byName :: String -> Annex (Remote Annex)
byName n = do
	res <- byName' n
	case res of
		Left e -> error e
		Right r -> return r
byName' :: String -> Annex (Either String (Remote Annex))
byName' "" = return $ Left "no remote specified"
byName' n = do
	allremotes <- genList
	let match = filter matching allremotes
	if (null match)
		then return $ Left $ "there is no git remote named \"" ++ n ++ "\""
		else return $ Right $ head match
	where
		matching r = n == name r || n == uuid r

{- Looks up a remote by name (or by UUID, or even by description),
 - and returns its UUID. -}
nameToUUID :: String -> Annex UUID
nameToUUID "." = getUUID =<< Annex.gitRepo -- special case for current repo
nameToUUID n = do
	res <- byName' n
	case res of
		Left e -> return . (maybe (error e) id) =<< byDescription
		Right r -> return $ uuid r
	where
		byDescription = return . M.lookup n . invertMap =<< uuidMap
		invertMap = M.fromList . map swap . M.toList
		swap (a, b) = (b, a)

{- Pretty-prints a list of UUIDs of remotes. -}
prettyPrintUUIDs :: [UUID] -> Annex String
prettyPrintUUIDs uuids = do
	here <- getUUID =<< Annex.gitRepo
	-- Show descriptions from the uuid log, falling back to remote names,
	-- as some remotes may not be in the uuid log.
	m <- liftM2 M.union uuidMap $
		return . M.fromList . map (\r -> (uuid r, name r)) =<< genList
	return $ unwords $ map (\u -> "\t" ++ prettify m u here ++ "\n") uuids
	where
		prettify m u here = base ++ ishere
			where
				base = if not $ null $ findlog m u
					then u ++ "  -- " ++ findlog m u
					else u
				ishere = if here == u then " <-- here" else ""
		findlog m u = M.findWithDefault "" u m

{- Filters a list of remotes to ones that have the listed uuids. -}
remotesWithUUID :: [Remote Annex] -> [UUID] -> [Remote Annex]
remotesWithUUID rs us = filter (\r -> uuid r `elem` us) rs

{- Filters a list of remotes to ones that do not have the listed uuids. -}
remotesWithoutUUID :: [Remote Annex] -> [UUID] -> [Remote Annex]
remotesWithoutUUID rs us = filter (\r -> uuid r `notElem` us) rs

{- Cost ordered lists of remotes that the LocationLog indicate may have a key.
 -}
keyPossibilities :: Key -> Annex [Remote Annex]
keyPossibilities key = return . fst =<< keyPossibilities' False key

{- Cost ordered lists of remotes that the LocationLog indicate may have a key.
 -
 - Also returns a list of UUIDs that are trusted to have the key
 - (some may not have configured remotes).
 -}
keyPossibilitiesTrusted :: Key -> Annex ([Remote Annex], [UUID])
keyPossibilitiesTrusted = keyPossibilities' True

keyPossibilities' :: Bool -> Key -> Annex ([Remote Annex], [UUID])
keyPossibilities' withtrusted key = do
	g <- Annex.gitRepo
	u <- getUUID g
	trusted <- if withtrusted then trustGet Trusted else return []

	-- get uuids of all remotes that are recorded to have the key
	uuids <- keyLocations key
	let validuuids = filter (/= u) uuids

	-- note that validuuids is assumed to not have dups
	let validtrusteduuids = intersect validuuids trusted

	-- remotes that match uuids that have the key
	allremotes <- genList
	let validremotes = remotesWithUUID allremotes validuuids

	return (sort validremotes, validtrusteduuids)

forceTrust :: TrustLevel -> String -> Annex ()
forceTrust level remotename = do
	r <- Remote.nameToUUID remotename
	Annex.changeState $ \s ->
		s { Annex.forcetrust = (r, level):Annex.forcetrust s }

{- Filename of remote.log. -}
remoteLog :: FilePath
remoteLog = "remote.log"

{- Adds or updates a remote's config in the log. -}
configSet :: UUID -> RemoteConfig -> Annex ()
configSet u c = do
	m <- readRemoteLog
	Branch.change remoteLog $ unlines $ sort $
		map toline $ M.toList $ M.insert u c m
	where
		toline (u', c') = u' ++ " " ++ (unwords $ configToKeyVal c')

{- Map of remotes by uuid containing key/value config maps. -}
readRemoteLog :: Annex (M.Map UUID RemoteConfig)
readRemoteLog = return . remoteLogParse =<< Branch.get remoteLog

remoteLogParse :: String -> M.Map UUID RemoteConfig
remoteLogParse s =
	M.fromList $ catMaybes $ map parseline $ filter (not . null) $ lines s
	where
		parseline l
			| length w > 2 = Just (u, c)
			| otherwise = Nothing
			where
				w = words l
				u = w !! 0
				c = keyValToConfig $ tail w

{- Given Strings like "key=value", generates a RemoteConfig. -}
keyValToConfig :: [String] -> RemoteConfig
keyValToConfig ws = M.fromList $ map (/=/) ws
	where
		(/=/) s = (k, v)
			where
				k = takeWhile (/= '=') s
				v = configUnEscape $ drop (1 + length k) s

configToKeyVal :: M.Map String String -> [String]
configToKeyVal m = map toword $ sort $ M.toList m
	where
		toword (k, v) = k ++ "=" ++ configEscape v

configEscape :: String -> String
configEscape = (>>= escape)
	where
		escape c
			| isSpace c || c `elem` "&" = "&" ++ show (ord c) ++ ";"
			| otherwise = [c]

configUnEscape :: String -> String
configUnEscape = unescape
	where
		unescape [] = []
		unescape (c:rest)
			| c == '&' = entity rest
			| otherwise = c : unescape rest
		entity s = if ok
				then chr (read num) : unescape rest
				else '&' : unescape s
			where
				num = takeWhile isNumber s
				r = drop (length num) s
				rest = drop 1 r
				ok = not (null num) && 
					not (null r) && r !! 0 == ';'

{- for quickcheck -}
prop_idempotent_configEscape :: String -> Bool
prop_idempotent_configEscape s = s == (configUnEscape $ configEscape s)
