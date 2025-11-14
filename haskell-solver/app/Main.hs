{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

import Control.Applicative
import Control.Monad
import Data.Bits

data BitVecError = EmptyError | TooManyBitsRequested Int deriving (Show)

type MaskedBitVec = Int -> Either BitVecError Integer

instance Alternative (Either BitVecError) where
  empty = Left EmptyError
  Left a <|> Left _ = Left a
  Left _ <|> b = b
  a <|> _ = a

mask :: Int -> Integer
mask n = (shiftL 1 n) - 1

limit :: Int -> Integer -> Integer
limit n val = val .&. mask n

bitvecXor :: MaskedBitVec -> MaskedBitVec -> MaskedBitVec
bitvecXor a b n = xor <$> a n <*> b n

bitvecOr :: MaskedBitVec -> MaskedBitVec -> MaskedBitVec
bitvecOr a b n =
  let (a', b') = (a n, b n)
   in ((.|.) <$> a' <*> b')
        <|> ( do
                av <- a'
                guard $ av == mask n
                return av
            )
        <|> ( do
                bv <- b'
                guard $ bv == mask n
                return bv
            )

bitVecVal :: Integer -> MaskedBitVec
bitVecVal value n = Right $ limit n value

bitvecAdd :: MaskedBitVec -> MaskedBitVec -> MaskedBitVec
bitvecAdd a b n = (\av bv -> limit n (av + bv)) <$> a n <*> b n

bitvecMul :: MaskedBitVec -> MaskedBitVec -> MaskedBitVec
bitvecMul a b n = (\av bv -> limit n (av * bv)) <$> a n <*> b n

bitvecLShL :: MaskedBitVec -> Int -> MaskedBitVec
bitvecLShL a shift n = do
  av <- a (max 0 (n - shift))
  return $ shiftL av shift

bitvecLShR :: MaskedBitVec -> Int -> MaskedBitVec
bitvecLShR a shift n = do
  av <- a (n + shift)
  return $ shiftR av shift

bitvecExtend :: MaskedBitVec -> Int -> Int -> MaskedBitVec
bitvecExtend x from to n =
  if n > from
    then do
      xv <- x from
      let m = shiftL ((shiftL 1 (to - from)) - 1) from
      return $ xv .|. m
    else x n

bitvecTruncate :: MaskedBitVec -> Int -> MaskedBitVec
bitvecTruncate x to n =
  if n > to
    then
      Left $ TooManyBitsRequested $ n - to
    else
      x n


multiplier :: MaskedBitVec
multiplier = bitVecVal 0x5DEECE66D

addend :: MaskedBitVec
addend = bitVecVal 0xB

-- Configuration
-- seed = bitVecVal 123123123123123

chunkX = bitVecVal 7

chunkZ = bitVecVal 16

seedXsigned = False

seedZsigned = False

-- Configuration end

seedToChunkseed :: MaskedBitVec -> MaskedBitVec
seedToChunkseed seed = chunkseed
  where
    seed0 = bitvecXor seed multiplier
    seed1 = bitvecAdd (bitvecMul seed0 multiplier) addend
    seed2 = bitvecAdd (bitvecMul seed1 multiplier) addend
    seed3 = bitvecAdd (bitvecMul seed2 multiplier) addend
    seed4 = bitvecAdd (bitvecMul seed3 multiplier) addend
    seedX'1 = bitvecTruncate ((seed1 `bitvecLShR` 16) `bitvecLShL` 32) 48
    seedX'2 = bitvecExtend (seed2 `bitvecLShR` 16) 32 48
    seedX' = bitvecAdd seedX'1 seedX'2
    seedX = let x = bitvecOr seedX' (bitVecVal 1) in if seedXsigned then bitvecAdd x (bitVecVal 2) else x
    seedZ'1 = bitvecTruncate ((seed3 `bitvecLShR` 16) `bitvecLShL` 32) 48
    seedZ'2 = bitvecExtend (seed4 `bitvecLShR` 16) 32 48
    seedZ' = bitvecAdd seedZ'1 seedZ'2
    seedZ = let x = bitvecOr seedZ' (bitVecVal 1) in if seedZsigned then bitvecAdd x (bitVecVal 2) else x
    a = bitvecMul chunkX seedX
    b = bitvecMul chunkZ seedZ
    chunkseed = bitvecXor (bitvecAdd a b) seed

fromRight' :: Either a b -> b
fromRight' (Right b) = b

seedToChunkseed' :: Integer -> Integer
seedToChunkseed' seed = fromRight' (seedToChunkseed (bitVecVal seed) 48)

type Step = Int

type Chunkseed = Integer

type PartialSeed = Integer

type Seed = Integer

crack :: Step -> Chunkseed -> PartialSeed -> Seed
crack 31 _ seed = seed
crack step chunkseed seed =
  let -- predicted last (step + 2) bits of chunkseed, if next bit would be 0
      chunkseed' = fromRight' $ seedToChunkseed (bitvecTruncate (bitVecVal seed) (17 + step + 1)) (2 + step)
   in if chunkseed' == limit (2 + step) chunkseed
        then
          -- last bit being 0 worked. let's go with that
          crack (step + 1) chunkseed seed
        else
          -- last bit being 0 didn't work. might be 1 (hopefully)
          crack (step + 1) chunkseed (seed .|. (shiftL 1 (17 + step)))

allPossibleSeeds :: [Seed]
allPossibleSeeds = do
  partial <- [0 .. mask 17]
  let seed = crack 0 45169098967850 partial
  guard $ seedToChunkseed' seed == 45169098967850
  return seed

main :: IO ()
main = do
  putStrLn "Cracking..."
  print $ length $ allPossibleSeeds
  print allPossibleSeeds

-- 0b11011111111101011010110000001000111001110110011