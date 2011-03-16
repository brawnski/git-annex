{- git-annex upgrade support
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Upgrade where

import Types
import Version
import qualified Upgrade.V0
import qualified Upgrade.V1

{- Uses the annex.version git config setting to automate upgrades. -}
upgrade :: Annex Bool
upgrade = do
	version <- getVersion
	case version of
		"0" -> Upgrade.V0.upgrade
		"1" -> Upgrade.V1.upgrade
		v | v == currentVersion -> return True
		_ -> error "this version of git-annex is too old for this git repository!"
