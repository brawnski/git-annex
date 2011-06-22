{- git-annex branch data types
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Branch where

data BranchCache = BranchCache {
	cachedFile :: Maybe FilePath,
	cachedContent :: String
}

emptyBranchCache :: BranchCache
emptyBranchCache = BranchCache Nothing ""
