{- git-annex dashed options
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Options where

import System.Console.GetOpt

import qualified Annex
import Types
import Command

{- Each dashed command-line option results in generation of an action
 - in the Annex monad that performs the necessary setting.
 -}
type Option = OptDescr (Annex ())

storeOptBool :: FlagName -> Bool -> Annex ()
storeOptBool name val = Annex.flagChange name $ FlagBool val
storeOptString :: FlagName -> String -> Annex ()
storeOptString name val = Annex.flagChange name $ FlagString val

commonOptions :: [Option]
commonOptions =
	[ Option ['f'] ["force"] (NoArg (storeOptBool "force" True))
		"allow actions that may lose annexed data"
	, Option ['q'] ["quiet"] (NoArg (storeOptBool "quiet" True))
		"avoid verbose output"
	, Option ['v'] ["verbose"] (NoArg (storeOptBool "quiet" False))
		"allow verbose output"
	, Option ['b'] ["backend"] (ReqArg (storeOptString "backend") paramName)
		"specify default key-value backend to use"
	]
