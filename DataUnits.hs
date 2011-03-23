{- data size display
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module DataUnits (roughSize) where

{- And now a rant: 
 -
 - In the beginning, we had powers of two, and they were good.
 -
 - Disk drive manufacturers noticed that some powers of two were
 - sorta close to some powers of ten, and that rounding down to the nearest
 - power of ten allowed them to advertise their drives were bigger. This
 - was sorta annoying.
 -
 - Then drives got big. Really, really big. This was good.
 -
 - Except that the small rounding error perpretrated by the drive
 - manufacturers suffered the fate of a small error, and became a large
 - error. This was bad.
 -
 - So, a committee was formed. And it arrived at a committee-like decision,
 - which satisfied noone, confused everyone, and made the world an uglier
 - place. As with all committees, this was meh.
 -
 - And the drive manufacturers happily continued selling drives that are
 - increasingly smaller than you'd expect, if you don't count on your
 - fingers. But that are increasingly bigger.
 -
 - Thus, I use units here that I loathe. Because if I didn't, people would
 - be confused that their drives seem the wrong size, and other people would
 - complain at me for not being standards compliant. And we call this
 - progress?
 -}

{- approximate display of a particular number of bytes -}
roughSize :: Bool -> Integer -> String
roughSize short i
	| i < 0 = "-" ++ roughSize short (negate i)
	| i >= at 8 = units 8 "yottabyte" "YB"
	| i >= at 7 = units 7 "zettabyte" "ZB"
	| i >= at 6 = units 6 "exabyte" "EB"
	| i >= at 5 = units 5 "petabyte" "PB"
	| i >= at 4 = units 4 "terabyte" "TB"
	| i >= at 3 = units 3 "gigabyte" "GB"
	| i >= at 2 = units 2 "megabyte" "MB"
	| i >= at 1 = units 1 "kilobyte" "kB"
	| otherwise = units 0 "byte" "B"
	where
		at :: Integer -> Integer
		at n = 1000^n

		chop :: Integer -> Integer
		chop d = round $ (fromInteger i :: Double) / fromInteger (at d)

		units d u u' = let num = chop d in
			show num ++ " " ++
			(if short then u' else plural num u)

		plural n u
			| n == 1 = u
			| otherwise = u ++ "s"
