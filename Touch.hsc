{- More control over touching a file.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE ForeignFunctionInterface #-}

module Touch (
	TimeSpec(..),
	nowTime,
	omitTime,
	touchBoth,
	touch
) where

import Foreign
import Foreign.C

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE >= 200809L
#endif

data TimeSpec = TimeSpec CTime CLong

touch :: FilePath -> TimeSpec -> Bool -> IO ()
touch file mtime follow = touchBoth file omitTime mtime follow

touchBoth :: FilePath -> TimeSpec -> TimeSpec -> Bool -> IO ()

omitTime :: TimeSpec
nowTime :: TimeSpec

#if (defined UTIME_OMIT && defined UTIME_NOW && defined AT_FDCWD && defined AT_SYMLINK_NOFOLLOW)

at_fdcwd :: CInt
at_fdcwd = #const AT_FDCWD

at_symlink_nofollow :: CInt
at_symlink_nofollow = #const AT_SYMLINK_NOFOLLOW

omitTime = TimeSpec 0 #const UTIME_OMIT
nowTime = TimeSpec 0 #const UTIME_NOW

instance Storable TimeSpec where
	-- use the larger alignment of the two types in the struct
	alignment _ = max sec_alignment nsec_alignment
		where
			sec_alignment = alignment (undefined::CTime)
			nsec_alignment = alignment (undefined::CLong)
	sizeOf _ = #{size struct timespec}
	peek ptr = do
		sec <- #{peek struct timespec, tv_sec} ptr
		nsec <- #{peek struct timespec, tv_nsec} ptr
		return $ TimeSpec sec nsec
	poke ptr (TimeSpec sec nsec) = do
		#{poke struct timespec, tv_sec} ptr sec
		#{poke struct timespec, tv_nsec} ptr nsec

{- While its interface is beastly, utimensat is in recent
   POSIX standards, unlike futimes. -}
foreign import ccall "utimensat" 
	c_utimensat :: CInt -> CString -> Ptr TimeSpec -> CInt -> IO CInt

{- Changes the access and/or modification times of an existing file.
   Can follow symlinks, or not. Throws IO error on failure. -}
touchBoth file atime mtime follow = 
	allocaArray 2 $ \ptr ->
	withCString file $ \f -> do
		pokeArray ptr [atime, mtime]
		r <- c_utimensat at_fdcwd f ptr flags
		if (r /= 0)
			then throwErrno "touchBoth"
			else return ()
	where
		flags = if follow
			then 0
			else at_symlink_nofollow 

#else
#warning "utimensat not available; building without symlink timestamp preservation support"
omitTime = TimeSpec 0 (-1)
nowTime = TimeSpec 0 (-2)
touchBoth _ _ _ _ = return ()
#endif
