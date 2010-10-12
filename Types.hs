{- git-annex core data types -}

module Types (
	State(..)
) where

import BackendType
import GitRepo

-- git-annex's runtime state
data State = State {
	repo :: GitRepo,
	backends :: [Backend]
}
