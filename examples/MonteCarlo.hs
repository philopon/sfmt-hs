{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import System.Random.SFMT

montePi :: Int -> GenIO -> IO Double
montePi len gen = loop (0::Int) (0::Int)
  where
    loop :: Int -> Int -> IO Double
    loop !i !cnt
        | i >= len  = return $ 4 * (fromIntegral cnt / fromIntegral len)
        | otherwise = do
            x <- uniform gen
            y <- uniform gen
            if sqrt (x ^ (2::Int) + y ^ (2::Int)) < (1 :: Double)
                then loop (i+1) (cnt+1)
                else loop (i+1) cnt

main :: IO ()
main = do
    gen1 <- initializeFromSeed 0
    gen2 <- initializeFromSeed 1
    gen3 <- initialize [0,5,2,8,5,1]
    gen4 <- initializeFromByteString "sd;bardnbogpwefvnpa"
    seed <- save gen4
    gen5 <- restore seed
    montePi 10000000 gen1 >>= print
    montePi 10000000 gen2 >>= print
    montePi 10000000 gen3 >>= print
    montePi 10000000 gen4 >>= print
    montePi 10000000 gen5 >>= print
    withSystemRandom . asGenIO $ \gen6 ->
        montePi 10000000 gen6 >>= print
