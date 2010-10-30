{- git-annex - Nasty portability workarounds. -}
module Portability where

-- old ghc does not know about SomeException. 
--
-- http://haskell.1045720.n5.nabble.com/Help-using-catch-in-6-10-td3127921.html#a3127921
--
-- This needs ghc -cpp
--
-- This would be better, but then users of old ghc would need to install it.
-- http://hackage.haskell.org/package/extensible-exceptions
#if __GLASGOW_HASKELL__ < 610
type SomeException = Exception 
#endif 
