{- More control over touching a file.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE ForeignFunctionInterface #-}

module Touch (
	TimeSpec(..),
	now,
	omit,
	touchBoth,
	touch
) where

import Foreign
import Foreign.C

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

data TimeSpec = TimeSpec CTime CLong

instance Storable TimeSpec where
	-- use the larger alignment of the two types in the struct
	alignment _ = max sec_alignment nsec_alignment
		where
			sec_alignment = alignment $ undefined::CTime
			nsec_alignment = alignment $ undefined::CLong
	sizeOf _ = #{size struct timespec}
	peek ptr = do
		sec <- #{peek struct timespec, tv_sec} ptr
		nsec <- #{peek struct timespec, tv_nsec} ptr
		return $ TimeSpec sec nsec
	poke ptr (TimeSpec sec nsec) = do
		#{poke struct timespec, tv_sec} ptr sec
		#{poke struct timespec, tv_nsec} ptr nsec

{- special timespecs -}
omit :: TimeSpec
omit = TimeSpec 0 #const UTIME_OMIT
now :: TimeSpec
now = TimeSpec 0 #const UTIME_NOW

{- While its interface is beastly, utimensat is in recent
   POSIX standards, unlike futimes. -}
foreign import ccall "utimensat" 
	c_utimensat :: CInt -> CString -> Ptr TimeSpec -> CInt -> IO CInt

{- Changes the access and/or modification times of a file.
   Can follow symlinks, or not. -}
touchBoth :: FilePath -> TimeSpec -> TimeSpec -> Bool -> IO Bool
touchBoth file atime mtime follow = 
	allocaArray 2 $ \ptr ->
	withCString file $ \f -> do
		pokeArray ptr [atime, mtime]
		r <- c_utimensat at_fdcwd f ptr flags
		return (r == 0)
	where
		at_fdcwd = #const AT_FDCWD
		at_symlink_nofollow = #const AT_SYMLINK_NOFOLLOW

		flags = if follow
			then 0
			else at_symlink_nofollow 

touch :: FilePath -> TimeSpec -> Bool -> IO Bool
touch file mtime follow = touchBoth file omit mtime follow
