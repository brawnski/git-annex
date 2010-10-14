{- git-annex data types, abstract only -}

module AbstractTypes (
	Annex,
	AnnexState,
	makeAnnexState,
	runAnnexState,
	gitAnnex,
	gitAnnexChange,
	backendsAnnex,
	backendsAnnexChange,

	Key,
	Backend
) where

import Types
