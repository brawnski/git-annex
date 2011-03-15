{- git-annex Key data type
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Key where

import Data.String.Utils
import Test.QuickCheck
import Data.Maybe
import Data.List

{- A Key has a unique name, is associated with a backend,
 - and may contain other metadata. -}
data Field = KeyName | KeyBackend | KeySize | KeyModTime
	deriving (Eq, Ord, Show)
newtype Key = Key [(Field, String)]
	deriving (Eq, Ord)

{- Generates a Key given a name, a backend and a list of other metadata. -}
keyGen :: String -> String -> [(Field, String)] -> Key
keyGen name backend meta = Key $ (KeyName, name):(KeyBackend, backend):meta

{- Gets the name of a Key. -}
keyName :: Key -> String
keyName key = fromJust $ keyField key KeyName

{- Gets the backend associated with a Key. -}
keyBackend :: Key -> String
keyBackend key = fromJust $ keyField key KeyBackend

{- Looks up a given Field of a Key's metadata. -}
keyField :: Key -> Field -> Maybe String
keyField (Key meta) field =
	if null matches
		then Nothing
		else Just $ snd $ head matches
	where
		matches = filter match meta
		match (f, _) = f == field

fieldSep :: Char
fieldSep = ','

{- Keys show as strings that are suitable for use as filenames.
 - The name field is always shown last, and is the only field
 - allowed to contain the fieldSep. -}
instance Show Key where
	show k@(Key meta) = join [fieldSep] $ map showp meta' ++ [name]
		where
			name = 'n':keyName k
			meta' = sort $ (filter (\(f, _) -> f /= KeyName)) meta
			showp (f, v) = (field f) : v

			field KeyBackend = 'b'
			field KeySize = 's'
			field KeyModTime = 'm'
			field f = error $ "unknown key field" ++ show f

instance Read Key where
	readsPrec _ s = [(Key (findfields s []), "")]
		where
			findfields ('n':v) m = (KeyName, v):m -- rest is name
			findfields (c:v) m =
				case span (/= fieldSep) v of
					(v', _:r) -> findfields r (field c v' m)
					_ -> m
			findfields [] m = m
			
			field 'b' v m = (KeyBackend, v):m
			field 's' v m = (KeySize, v):m
			field 'm' v m = (KeyModTime, v):m
			field _ _ m = m

-- for quickcheck
instance Arbitrary Key where
	arbitrary = do
		backendname <- arbitrary
		value <- arbitrary
		return $ keyGen value backendname []

prop_idempotent_key_read_show :: Key -> Bool
prop_idempotent_key_read_show k
	-- backend names will never contain the fieldSep
	| fieldSep `elem` (keyBackend k) = True
	| otherwise = k == (read $ show k)
