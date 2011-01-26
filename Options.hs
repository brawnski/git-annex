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

commonOptions :: [Option]
commonOptions =
	[ Option ['f'] ["force"] (NoArg (setforce True))
		"allow actions that may lose annexed data"
	, Option ['q'] ["quiet"] (NoArg (setquiet True))
		"avoid verbose output"
	, Option ['v'] ["verbose"] (NoArg (setquiet False))
		"allow verbose output"
	, Option ['b'] ["backend"] (ReqArg setdefaultbackend paramName)
		"specify default key-value backend to use"
	]
	where
		setforce v = Annex.changeState $ \s -> s { Annex.force = v }
		setquiet v = Annex.changeState $ \s -> s { Annex.quiet = v }
		setdefaultbackend v = Annex.changeState $ \s -> s { Annex.defaultbackend = Just v }
