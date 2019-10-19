{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module LExAu.Utilities.DescriptionLength
  ( binaryEncodeInteger
  , bitLength
  , test
  ) where

import Data.Bits
import Data.Foldable (toList)
import Data.Ratio (approxRational, denominator, numerator, (%))
import Data.Sequence (Seq, singleton, (|>))
import Text.Show (showListWith)

import LExAu.API.DescriptionLength (
    DescriptionLength(descriptionLength),
    PrefixEncode(prefixEncode),
    toTerseCode
  )
import LExAu.Utilities.Logging (logVerbose, showListVertically)

decomposeChunks :: Int -> Integer -> Integer -> (Integer, Integer)
decomposeChunks chunkSize chunkLimit n =
  if n < chunkLimit
    then (0, n)
    else
      let (chunks, remainder) = decomposeChunks chunkSize chunkLimit (n `shiftR` chunkSize)
      in (chunks + 1, remainder)

bitLength :: Integer -> Integer
bitLength n = 
  let chunkSize = (bitSize (1 :: Int)) - 2
      chunkLimit = 1 `shift` chunkSize
      (chunks, remainder) = decomposeChunks chunkSize chunkLimit n
      remainderSize = truncate $ logBase 2.0 $ encodeFloat remainder 0
  in chunks * (toInteger chunkSize) + remainderSize + 1

binaryEncodeInteger :: Integer -> String
binaryEncodeInteger = toList . binarySeqEncodeInteger

binarySeqEncodeInteger :: Integer -> Seq Char
binarySeqEncodeInteger n =
  if n < 1
    then error $ "Binary encode integer: The number should be positive " ++ (show n)
    else
      if n == 1
        then singleton 'I'
        else let bit =
                   if n `mod` 2 == 0
                     then 'O'
                     else 'I'
             in (binarySeqEncodeInteger $ n `div` 2) |> bit

prefixEncodeInteger :: Bool -> Integer -> ShowS
prefixEncodeInteger lastChunk n =
  if n < 1
    then error $ "Prefix encode integer: The number should be positive " ++ (show n)
    else
      let flag =
            if lastChunk
              then 'I'
              else 'O'
      in if n == 1
           then showChar flag . showString "(1)"
           else
             let (one : remainder) = binaryEncodeInteger n
             in prefixEncodeInteger False (toInteger $ length remainder) . showChar '/' . showChar flag . showString ":1" . showString remainder . showChar '(' . shows n . showChar ')'

instance PrefixEncode Integer where
  prefixEncode = prefixEncodeInteger True . (1+)

instance PrefixEncode Int where
  prefixEncode = prefixEncode . toInteger

internalDescriptionLength :: Integer -> Integer
internalDescriptionLength n =
  if n < 1
    then error $ "Description length integer: The number should be positive " ++ (show n)
    else
      if n == 1
        then 1
        else (internalDescriptionLength $ (bitLength n) - 1) + (bitLength n)

instance DescriptionLength Integer where
  descriptionLength n = internalDescriptionLength (n + 1)

instance DescriptionLength Rational where
  descriptionLength r = (descriptionLength $ numerator r) + (descriptionLength $ denominator r)

instance PrefixEncode Rational where
  prefixEncode rational =
    showString "<" .
    prefixEncodeInteger True ((numerator rational) + 1) .
    showString " over " .
    prefixEncodeInteger True ((denominator rational) + 1) .
    showString ">"

testDescriptionLength :: Integer -> IO ()
testDescriptionLength integer =
  let prefixCode = (showString "<" . prefixEncodeInteger True (integer + 1)) ">"
      terseCode = toTerseCode prefixCode
  in logVerbose "Description length" [
         shows integer,
         shows $ integer + 1,
         shows $ bitLength (integer + 1),
         showString prefixCode,
         showString terseCode,
         shows $ length terseCode,
         shows $ descriptionLength integer
       ]

testDescriptionLengthRational :: Rational -> IO ()
testDescriptionLengthRational rational =
  let prefixCode = (
          showString "<" .
          prefixEncodeInteger True ((numerator rational) + 1) .
          showString " over " .
          prefixEncodeInteger True ((denominator rational) + 1)
        ) ">"
      terseCode = toTerseCode prefixCode
  in logVerbose "Description length" [
         shows rational,
         showString prefixCode,
         showString terseCode,
         shows $ length terseCode,
         shows $ descriptionLength rational
       ]

test :: IO ()
test =
  do putStrLn $ ""
     putStrLn $ "Test LExAu.Utilities.DescriptionLength"
     foldr (>>) (return ()) $ map testDescriptionLength [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 14, 15, 19, 30, 31, 62, 63, 100, 1000, 65534, 65535]
     foldr (>>) (return ()) $ map testDescriptionLengthRational [13 % 65534, 19 % 65535]

-- EOF
