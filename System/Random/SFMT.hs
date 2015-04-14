{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}

module System.Random.SFMT
    ( -- * Gen
      Gen
    , initializeFromSeed, create, initialize, initializeFromByteString
    , withSystemRandom, createSystemRandom
      -- ** Type helpers
    , GenIO, GenST
    , asGenIO, asGenST
      -- * Variates
    , Variate(..)
      -- * Seed
    , Seed
    , unsafeFromSeed, unsafeToSeed
    , save, restore
    ) where

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import System.Random.SFMT.Foreign
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import qualified Data.Foldable as F
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as S
import System.Entropy
import Data.Int
import Data.Word
import Data.Bits
import Unsafe.Coerce
import System.IO.Unsafe

#if !MIN_VERSION_primitive(0,6,0)
#define PrimBase PrimMonad
#endif

newtype Gen s = Gen (ForeignPtr SFMT)
instance Show (Gen s) where
    show = unsafePerformIO . getIDString

getIDString :: Gen s -> IO String
getIDString (Gen gen) = withForeignPtr gen $ \ptr ->
    sfmt_get_idstring ptr >>= peekCString

initializeFromSeed :: PrimMonad m => Int -> m (Gen (PrimState m))
initializeFromSeed seed = unsafePrimToPrim $ do
    bytes <- mallocBytes sizeOfSFMT
    sfmt_init_gen_rand bytes (fromIntegral seed)
    Gen `liftM` newForeignPtr finalizerFree bytes

create :: PrimMonad m => m (Gen (PrimState m))
create = initializeFromSeed 0

initialize :: (PrimMonad m, F.Foldable f) => f Word -> m (Gen (PrimState m))
initialize v = unsafePrimToPrim . withArray (unsafeCoerce $ F.toList v) $ \ptr -> do
    bytes <- mallocBytes sizeOfSFMT
    let len = F.foldl' (\i _ -> i + 1) 0 v
    sfmt_init_by_array bytes ptr len
    Gen `liftM` newForeignPtr finalizerFree bytes

initializeFromByteString :: PrimMonad m => S.ByteString -> m (Gen (PrimState m))
initializeFromByteString bs = unsafePrimToPrim . S.unsafeUseAsCStringLen bs $ \(ptr, len) -> do
    bytes <- mallocBytes sizeOfSFMT
    sfmt_init_by_array bytes (castPtr ptr) (fromIntegral $ len `quot` 4)
    Gen `liftM` newForeignPtr finalizerFree bytes

withSystemRandom :: PrimBase m => (Gen (PrimState m) -> m a) -> IO a
withSystemRandom m = do
    bs  <- getEntropy (constSFMT_N * 16)
    gen <- initializeFromByteString bs
    unsafePrimToIO $ m (unsafeCoerce gen)

createSystemRandom :: IO GenIO
createSystemRandom = withSystemRandom (return :: GenIO -> IO GenIO)

type GenIO   = Gen (PrimState IO)
type GenST s = Gen (PrimState (ST s))

asGenIO :: (GenIO -> IO a) -> GenIO -> IO a
asGenIO = id

asGenST :: (GenST s -> ST s a) -> GenST s -> ST s a
asGenST = id

genRand :: PrimMonad m => (Ptr SFMT -> IO a) -> Gen (PrimState m) -> m a
genRand f (Gen gen) = unsafePrimToPrim $ withForeignPtr gen f

genRandWord32 :: PrimMonad m => Gen (PrimState m) -> m Word32
genRandWord32 g = fromIntegral `liftM` genRand wrap_genrand_uint32 g

genRandWord64 :: PrimMonad m => Gen (PrimState m) -> m Word64
genRandWord64 g = fromIntegral `liftM` genRand wrap_genrand_uint64 g

genRandReal2 :: PrimMonad m => Gen (PrimState m) -> m Float
genRandReal2 g = realToFrac `liftM` genRand wrap_genrand_real2 g

genRandRes53 :: PrimMonad m => Gen (PrimState m) -> m Double
genRandRes53 g = realToFrac `liftM` genRand wrap_genrand_res53 g

class Variate a where
    uniform       :: PrimMonad m => Gen (PrimState m) -> m a
    uniformR      :: PrimMonad m => (a, a) -> Gen (PrimState m) -> m a

instance Variate Bool where
    uniform g = (\i -> i .&. 1 /= 0) `liftM` genRandWord32 g
    uniformR (False,True)  g = uniform g
    uniformR (False,False) _ = return False
    uniformR (True,True)   _ = return True
    uniformR (True,False)  g = uniform g
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Float where
    uniform            = genRandReal2
    uniformR (x1,x2) g = (\d -> x1 + (x2-x1) * d) `liftM` genRandReal2 g
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Double where
    uniform            = genRandRes53
    uniformR (x1,x2) g = (\d -> x1 + (x2-x1) * d) `liftM` genRandRes53 g
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Word where
#if WORD_SIZE_IN_BITS < 64
    uniform g = fromIntegral `liftM` genRandWord32 g
    uniformR  = uniformRange (undefined :: Word32)
#else
    uniform g = fromIntegral `liftM` genRandWord64 g
    uniformR  = uniformRange (undefined :: Word64)
#endif
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Word8 where
    uniform g = fromIntegral `liftM` genRandWord32 g
    uniformR  = uniformRange (undefined :: Word8)
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Word16 where
    uniform g = fromIntegral `liftM` genRandWord32 g
    uniformR  = uniformRange (undefined :: Word16)
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Word32 where
    uniform  = genRandWord32
    uniformR = uniformRange (undefined :: Word32)
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Word64 where
    uniform  = genRandWord64
    uniformR = uniformRange (undefined :: Word64)
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Int where
#if WORD_SIZE_IN_BITS < 64
    uniform g = fromIntegral `liftM` genRandWord32 g
    uniformR  = uniformRange (undefined :: Word32)
#else
    uniform g = fromIntegral `liftM` genRandWord64 g
    uniformR  = uniformRange (undefined :: Word64)
#endif
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Int8 where
    uniform g = fromIntegral `liftM` genRandWord32 g
    uniformR  = uniformRange (undefined :: Word8)
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Int16 where
    uniform g = fromIntegral `liftM` genRandWord32 g
    uniformR  = uniformRange (undefined :: Word16)
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Int32 where
    uniform g = fromIntegral `liftM` genRandWord32 g
    uniformR  = uniformRange (undefined :: Word32)
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Int64 where
    uniform g = fromIntegral `liftM` genRandWord64 g
    uniformR  = uniformRange (undefined :: Word64)
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance (Variate a, Variate b) => Variate (a,b) where
    uniform g = (,) `liftM` uniform g `ap` uniform g
    uniformR ((x1,y1),(x2,y2)) g = (,) `liftM` uniformR (x1,x2) g `ap` uniformR (y1,y2) g
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

instance (Variate a, Variate b, Variate c) => Variate (a,b,c) where
    uniform g = (,,) `liftM` uniform g `ap` uniform g `ap` uniform g
    uniformR ((x1,y1,z1),(x2,y2,z2)) g =
      (,,) `liftM` uniformR (x1,x2) g `ap` uniformR (y1,y2) g `ap` uniformR (z1,z2) g
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

instance (Variate a, Variate b, Variate c, Variate d) => Variate (a,b,c,d) where
    uniform g = (,,,) `liftM` uniform g `ap` uniform g `ap` uniform g
                `ap` uniform g
    uniformR ((x1,y1,z1,t1),(x2,y2,z2,t2)) g =
      (,,,) `liftM` uniformR (x1,x2) g `ap` uniformR (y1,y2) g `ap`
                    uniformR (z1,z2) g `ap` uniformR (t1,t2) g
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

uniformRange :: forall m word a.
    (Variate word, Bounded word, Eq word, Num word, Integral word, Ord word
    , PrimMonad m, Variate a, Integral a, Show word)
    => word -> (a, a) -> Gen (PrimState m) -> m a
uniformRange _ = go
  where
    go (x1, x2) g
        | n == 0    = uniform g
        | otherwise = loop
      where
        ( i, j ) | x1 < x2   = ( x1, x2 )
                 | otherwise = ( x2, x1 )
        n = 1 + fromIntegral j - fromIntegral i :: word
        buckets = maxBound `div` n
        maxN    = buckets * n
        loop    = do
            x <- uniform g :: m word
            if x < maxN
                then return $! i + fromIntegral (x `div` buckets)
                else loop
{-# INLINE uniformRange #-}

newtype Seed = Seed { unsafeFromSeed :: S.ByteString }
    deriving Show

unsafeToSeed :: S.ByteString -> Seed
unsafeToSeed = Seed

save :: PrimMonad m => Gen (PrimState m) -> m Seed
save (Gen gen) = unsafePrimToPrim . withForeignPtr gen $ \ptr ->
    Seed `liftM` S.packCStringLen (castPtr ptr, sizeOfSFMT)

restore :: PrimMonad m => Seed -> m (Gen (PrimState m))
restore (Seed bs) = unsafePrimToPrim . S.unsafeUseAsCString bs $ \ptr -> do
    bytes <- mallocBytes sizeOfSFMT
    copyBytes bytes (castPtr ptr) sizeOfSFMT
    Gen `liftM` newForeignPtr finalizerFree bytes

-- Assertion failed: (sfmt->idx % 2 == 0), function sfmt_genrand_uint64, file SFMT-src-1.4.1/SFMT.h, line 158.
