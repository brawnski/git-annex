{- Simple Base64 access
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Base64 (toB64, fromB64) where

import Codec.Binary.Base64
import Data.Bits.Utils

toB64 :: String -> String		
toB64 = encode . s2w8

fromB64 :: String -> String
fromB64 s =
	case decode s of
		Nothing -> error "bad base64 encoded data"
		Just ws -> w82s ws
