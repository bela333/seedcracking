{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

import Control.Applicative
import Control.Monad
import Data.Bits
import Debug.Trace (trace)

data BitVec = BitVec {bits :: Integer, mask :: Integer, size :: Int} deriving (Show)

data BitVecError = UnknownValueError (Integer, Int) | InvalidSizeError | EmptyError | TooManyBitsRequested Int deriving (Show)

type MaskedBitVec = Int -> Either BitVecError BitVec

instance Alternative (Either BitVecError) where
  empty = Left EmptyError
  Left a <|> Left _ = Left a
  Left _ <|> b = b
  a <|> _ = a

guardComplete :: MaskedBitVec -> MaskedBitVec
guardComplete x n = do
  xv <- x n
  let m = shiftL (1 :: Integer) n - 1
  let maskedMask = mask xv .&. m
  if maskedMask == m
    then
      Right xv
    else
      Left $ UnknownValueError (maskedMask, n)

bitvecXor :: MaskedBitVec -> MaskedBitVec -> MaskedBitVec
bitvecXor a b n = do
  av <- guardComplete a n
  bv <- guardComplete b n
  return $ av{bits = xor (bits av) (bits bv)}

bitvecAnd :: MaskedBitVec -> MaskedBitVec -> MaskedBitVec
bitvecAnd a b n =
  ( do
      av <- guardComplete a n
      bv <- guardComplete b n
      return $ av{bits = bits av .&. bits bv}
  )
    <|> ( do
            av <- guardComplete a n
            let m = shiftL (1 :: Integer) n - 1
            guard $ bits av .&. m == 0
            return $ av{bits = 0}
        )
    <|> ( do
            bv <- guardComplete b n
            let m = shiftL (1 :: Integer) n - 1
            guard $ bits bv .&. m == 0
            return $ bv{bits = 0}
        )

bitvecOr :: MaskedBitVec -> MaskedBitVec -> MaskedBitVec
bitvecOr a b n =
  ( do
      av <- guardComplete a n
      bv <- guardComplete b n
      return $ av{bits = bits av .|. bits bv}
  )
    <|> ( do
            av <- guardComplete a n
            let m = shiftL (1 :: Integer) n - 1
            guard $ bits av .&. m == m
            return av
        )
    <|> ( do
            bv <- guardComplete b n
            let m = shiftL (1 :: Integer) n - 1
            guard $ bits bv .&. m == m
            return bv
        )

bitVecVal :: Integer -> Int -> MaskedBitVec
bitVecVal value size n =
  if n > size
    then Left (TooManyBitsRequested size)
    else
      Right $
        BitVec
          { bits = value .&. m
          , mask = m
          , size = size
          }
 where
  m = shiftL (1 :: Integer) n - 1

emptyBitVec :: Int -> MaskedBitVec
emptyBitVec size n = do
  v <- bitVecVal 0 size n
  return $ v{mask = 0}

setBit :: MaskedBitVec -> Int -> Bool -> MaskedBitVec
setBit x index b n =
  if index >= n
    then x n
    else
      ( do
          xv <- x n
          let m1 = shiftL (1 :: Integer) n - 1
          let m2 = shiftL (1 :: Integer) index
          let m = xor m1 m2
          let nbits = bits xv .&. m .|. if b then m2 else 0
          let nmask = mask xv .|. m2
          Right $
            xv
              { bits = nbits
              , mask = nmask
              }
      )

bitvecAdd :: MaskedBitVec -> MaskedBitVec -> MaskedBitVec
bitvecAdd a b n = do
  av <- guardComplete a n
  bv <- guardComplete b n
  guard $ size av == size bv
  bitVecVal (bits av + bits bv) (size av) n

bitvecMul :: MaskedBitVec -> MaskedBitVec -> MaskedBitVec
bitvecMul a b n = do
  av <- guardComplete a n
  bv <- guardComplete b n
  guard $ size av == size bv
  bitVecVal (bits av * bits bv) (size av) n

bitvecLShL :: MaskedBitVec -> Int -> MaskedBitVec
bitvecLShL a shift n = do
  av <- guardComplete a (max 0 (n - shift))
  bitVecVal (shiftL (bits av) shift) (size av + shift) n

bitvecLShR :: MaskedBitVec -> Int -> MaskedBitVec
bitvecLShR a shift n = do
  av <- guardComplete a (n + shift)
  bitVecVal (shiftR (bits av) shift) (size av - shift) n

bitvecExtend :: MaskedBitVec -> Int -> Int -> MaskedBitVec
bitvecExtend x from to n =
  if n > from
    then do
      xv <- guardComplete x from
      guard $ size xv == from
      let bs = bits xv
      let m = shiftL (shiftL (1 :: Integer) (to - from) - 1) from
      let signed = bs .&. shiftL (1 :: Integer) (from - 1) /= 0
      let bs' = if signed then bs .|. m else bs
      return $ xv{size = to, mask = mask xv .|. m, bits = bs'}
    else do
      xv <- x n
      return $ xv{size = to}

bitvecTruncate :: MaskedBitVec -> Int -> MaskedBitVec
bitvecTruncate x to n =
  if n > to
    then
      Left $ TooManyBitsRequested to
    else do
      let m = shiftL (1 :: Integer) to - 1
      xv <- x n
      guard $ size xv >= to
      return $ xv{bits = bits xv .&. m, mask = mask xv .&. m, size = to}

toSigned :: MaskedBitVec -> Int -> Either BitVecError Integer
toSigned x size' = do
  xv <- guardComplete x size'
  guard $ size xv == size'
  let value = bits xv
  let m = shiftL (1 :: Integer) (size' - 1)
  if value >= m
    then
      return $ value - 2 * m
    else
      return value

multiplier = bitVecVal 0x5DEECE66D 48
addend = bitVecVal 0xB 48

seed = bitvecExtend (bitvecTruncate (bitVecVal 123123123123123 48) 18) 18 48
chunkX = bitVecVal 7 48
chunkZ = bitVecVal 16 48
var7signed = False
var9signed = False

seed0 = bitvecXor seed multiplier
seed1 = bitvecAdd (bitvecMul seed0 multiplier) addend
seed2 = bitvecAdd (bitvecMul seed1 multiplier) addend
seed3 = bitvecAdd (bitvecMul seed2 multiplier) addend
seed4 = bitvecAdd (bitvecMul seed3 multiplier) addend

var7'1 = bitvecTruncate ((seed1 `bitvecLShR` 16) `bitvecLShL` 32) 48
var7'2 = bitvecExtend (seed2 `bitvecLShR` 16) 32 48
var7' = bitvecAdd var7'1 var7'2
var7 = let x = bitvecOr var7' (bitVecVal 1 48) in if var7signed then bitvecAdd x (bitVecVal 2 64) else x

var9'1 = bitvecTruncate ((seed3 `bitvecLShR` 16) `bitvecLShL` 32) 48
var9'2 = bitvecExtend (seed4 `bitvecLShR` 16) 32 48
var9' = bitvecAdd var9'1 var9'2
var9 = let x = bitvecOr var9' (bitVecVal 1 48) in if var9signed then bitvecAdd x (bitVecVal 2 48) else x

a = bitvecMul chunkX var7
b = bitvecMul chunkZ var9
chunkseed = bitvecXor (bitvecAdd a b) seed
