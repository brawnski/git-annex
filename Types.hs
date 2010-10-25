{- git-annex abstract data types -}

module Types (
	Annex,
	AnnexState,
	Backend,
	Key,
	genKey,
	backendName,
	keyName,
	FlagName,
	Flag(..)
) where

import TypeInternals
