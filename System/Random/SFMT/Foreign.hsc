{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#include <SFMT.h>

module System.Random.SFMT.Foreign where

import Foreign.Ptr
import Foreign.C

data SFMT

sizeOfSFMT :: Int
sizeOfSFMT = #size sfmt_t
{-# INLINE sizeOfSFMT #-}

constSFMT_N :: Int
constSFMT_N = #const SFMT_N
{-# INLINE constSFMT_N #-}

constSFMT_N32 :: Int
constSFMT_N32 = #const SFMT_N32
{-# INLINE constSFMT_N32 #-}

constSFMT_N64 :: Int
constSFMT_N64 = #const SFMT_N64
{-# INLINE constSFMT_N64 #-}

type CUInt32 = CUInt
type CUInt64 = CULLong

foreign import ccall unsafe sfmt_init_gen_rand
    :: Ptr SFMT -> CUInt32 -> IO ()

foreign import ccall unsafe sfmt_init_by_array
    :: Ptr SFMT -> Ptr CUInt32 -> CInt -> IO ()

foreign import ccall unsafe sfmt_get_idstring
    :: Ptr SFMT -> IO CString

foreign import ccall unsafe wrap_genrand_uint32
    :: Ptr SFMT -> IO CUInt32

foreign import ccall unsafe wrap_genrand_uint64
    :: Ptr SFMT -> IO CUInt64

foreign import ccall unsafe wrap_genrand_real2
    :: Ptr SFMT -> IO CDouble

foreign import ccall unsafe wrap_genrand_res53
    :: Ptr SFMT -> IO CDouble
