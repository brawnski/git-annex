{- git-annex abstract data types -}

module Types (
	Annex,
	AnnexState,
	Backend,
	Key,
	genKey,
	backendName,
	FlagName,
	Flag(..)
) where

import TypeInternals
