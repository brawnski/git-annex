{- git-annex Key data type
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Key (
	Key(..),
	stubKey,
	readKey,

	prop_idempotent_key_read_show
) where

import Test.QuickCheck
import Utility
import System.Posix.Types

{- A Key has a unique name, is associated with a key/value backend,
 - and may contain other optional metadata. -}
data Key = Key {
	keyName :: String,
	keyBackendName :: String,
	keySize :: Maybe Integer,
	keyMtime :: Maybe EpochTime
} deriving (Eq, Ord)

stubKey :: Key
stubKey = Key {
	keyName = "",
	keyBackendName = "",
	keySize = Nothing,
	keyMtime = Nothing
}

fieldSep :: Char
fieldSep = ','

{- Keys show as strings that are suitable for use as filenames.
 - The name field is always shown last, and is the only field
 - allowed to contain the fieldSep. -}
instance Show Key where
	show Key { keyBackendName = b, keySize = s, keyMtime = m, keyName = n } =
		('b' : b) +++ ('s' ?: s) +++ ('m' ?: m) +++ ('n' : n)
		where
			"" +++ y = y
			x +++ "" = x
			x +++ y = x ++ fieldSep:y
			c ?: (Just v) = c:(show v)
			_ ?: _ = ""

readKey :: String -> Maybe Key
readKey s = if key == Just stubKey then Nothing else key
	where
		key = findfields s $ Just stubKey

		findfields ('n':v) (Just k) = Just $ k { keyName = v }
		findfields (c:v) (Just k) =
			case span (/= fieldSep) v of
				(v', _:r) -> findfields r $ addfield k c v'
				_ -> Nothing
		findfields _ v = v
		
		addfield k 'b' v = Just k { keyBackendName = v }
		addfield k 's' v = Just k { keySize = readMaybe v }
		addfield k 'm' v = Just k { keyMtime = readMaybe v }
		addfield _ _ _ = Nothing

-- for quickcheck
instance Arbitrary Key where
	arbitrary = do
		n <- arbitrary
		b <- elements ['A'..'Z']
		s <- arbitrary
		return $ Key { keyName = n, keyBackendName = [b] , keySize = s }

prop_idempotent_key_read_show :: Key -> Bool
prop_idempotent_key_read_show k = Just k == (readKey $ show k)
