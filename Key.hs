{- git-annex Key data type
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Key where

import Test.QuickCheck
import Utility

{- A Key has a unique name, is associated with a backend,
 - and may contain other metadata. -}
data Key = Key {
	keyName :: String,
	keyBackend :: String,
	keySize :: Maybe Int,
	keyMtime :: Maybe Int
} deriving (Eq, Ord)

fieldSep :: Char
fieldSep = ','

{- Keys show as strings that are suitable for use as filenames.
 - The name field is always shown last, and is the only field
 - allowed to contain the fieldSep. -}
instance Show Key where
	show Key { keyBackend = b, keySize = s, keyMtime = m, keyName = n } =
		('b' : b) +++ ('s' ?: s) +++ ('m' ?: m) +++ ('n' : n)
		where
			"" +++ y = y
			x +++ "" = x
			x +++ y = x ++ fieldSep:y
			c ?: (Just v) = c:(show v)
			_ ?: _ = ""

readKey :: String -> Maybe Key
readKey s = if key == stub then Nothing else key
	where
		key = findfields s stub

		stub = Just Key {
			keyName = "",
			keyBackend = "",
			keySize = Nothing,
			keyMtime = Nothing
		}

		findfields ('n':v) (Just k) = Just $ k { keyName = v }
		findfields (c:v) (Just k) =
			case span (/= fieldSep) v of
				(v', _:r) -> findfields r $ addfield k c v'
				_ -> Nothing
		findfields _ v = v
		
		addfield k 'b' v = Just k { keyBackend = v }
		addfield k 's' v = Just k { keySize = readMaybe v }
		addfield k 'm' v = Just k { keyMtime = readMaybe v }
		addfield _ _ _ = Nothing

-- for quickcheck
instance Arbitrary Key where
	arbitrary = do
		n <- arbitrary
		b <- elements ['A'..'Z']
		s <- arbitrary
		m <- arbitrary
		return $ Key { keyName = n, keyBackend = [b] , keySize = s, keyMtime = m }

prop_idempotent_key_read_show :: Key -> Bool
prop_idempotent_key_read_show k = Just k == (readKey $ show k)
