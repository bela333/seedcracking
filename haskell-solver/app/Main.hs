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
mask n = shiftL 1 n - 1

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
      let m = shiftL (shiftL 1 (to - from) - 1) from
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

data Configuration = Configuration
  { chunkX :: MaskedBitVec
  , chunkZ :: MaskedBitVec
  , seedXsigned :: Bool
  , seedZsigned :: Bool
  }

-- Configuration end

seedStep :: MaskedBitVec -> MaskedBitVec
seedStep seed = bitvecAdd (bitvecMul seed multiplier) addend

getLong :: MaskedBitVec -> MaskedBitVec -> MaskedBitVec
getLong seed1 seed2 = seedX'
 where
  seedX'1 = bitvecTruncate ((seed1 `bitvecLShR` 16) `bitvecLShL` 32) 64
  seedX'2 = bitvecExtend (seed2 `bitvecLShR` 16) 32 64
  seedX' = bitvecAdd seedX'1 seedX'2

seedToChunkseed :: Configuration -> MaskedBitVec -> MaskedBitVec
seedToChunkseed Configuration{seedXsigned, seedZsigned, chunkX, chunkZ} seed = chunkseed
 where
  seed0 = bitvecXor seed multiplier
  seed1 = seedStep seed0
  seed2 = seedStep seed1
  seed3 = seedStep seed2
  seed4 = seedStep seed3
  seedX' = getLong seed1 seed2
  seedX'last = fromRight' (seedX' 1) == 1
  seedX = let x = bitvecOr seedX' (bitVecVal 1) in if seedXsigned && seedX'last then bitvecAdd x (bitVecVal 2) else x
  seedZ'last = fromRight' (seedZ' 1) == 1
  seedZ' = getLong seed3 seed4
  seedZ = let x = bitvecOr seedZ' (bitVecVal 1) in if seedZsigned && seedZ'last then bitvecAdd x (bitVecVal 2) else x
  a = bitvecMul chunkX seedX
  b = bitvecMul chunkZ seedZ
  chunkseed = bitvecTruncate (bitvecXor (bitvecAdd a b) seed) 48

fromRight' :: Either a b -> b
fromRight' (Right b) = b

seedToChunkseed' :: Configuration -> Integer -> Integer
seedToChunkseed' config seed = fromRight' (seedToChunkseed config (bitVecVal seed) 48)

type Step = Int

type Chunkseed = Integer

type PartialSeed = Integer

type Seed = Integer

crack :: Configuration -> Step -> Chunkseed -> PartialSeed -> Seed
crack config 31 _ seed = seed
crack config step chunkseed seed =
  let
    -- predicted last (step + 2) bits of chunkseed, if next bit would be 0
    chunkseed' = fromRight' $ seedToChunkseed config (bitvecTruncate (bitVecVal seed) (17 + step + 1)) (2 + step)
   in
    if chunkseed' == limit (2 + step) chunkseed
      then
        -- last bit being 0 worked. let's go with that
        crack config (step + 1) chunkseed seed
      else
        -- last bit being 0 didn't work. might be 1 (hopefully)
        crack config (step + 1) chunkseed (seed .|. shiftL 1 (17 + step))

findAllPossibleSeeds :: Chunkseed -> Integer -> Integer -> [Seed]
findAllPossibleSeeds chunkseed chunkX chunkZ = do
  partial <- [0 .. mask 17]
  seedXsigned <- [True, False]
  seedZsigned <- [True, False]
  let config = Configuration{chunkX = bitVecVal chunkX, chunkZ = bitVecVal chunkZ, seedXsigned = seedXsigned, seedZsigned = seedZsigned}
  let seed = crack config 0 chunkseed partial

  let seed0 = bitvecXor (bitvecTruncate (bitVecVal seed) 48) multiplier
  let seed1 = seedStep seed0
  let seed2 = seedStep seed1
  let seed3 = seedStep seed2
  let seed4 = seedStep seed3

  let seedX = fromRight' $ getLong seed1 seed2 64
  let seedZ = fromRight' $ getLong seed3 seed4 64

  let seedXsigned' = (seedX .&. shiftL 1 63) /= 0
  let seedZsigned' = (seedZ .&. shiftL 1 63) /= 0

  guard $ seedXsigned' == seedXsigned
  guard $ seedZsigned' == seedZsigned

  guard $ seedToChunkseed' config seed == chunkseed

  return seed

main :: IO ()
main = do
  let allPossibleSeeds = findAllPossibleSeeds 235749354401186 15 16
  putStrLn "Cracking..."
  print $ length allPossibleSeeds
  print allPossibleSeeds
