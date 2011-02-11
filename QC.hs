module QC where

import Com.DiagMessage -- needs to export constructors to be testable
import Com.HSFZMessage
import Data.Char
import Data.List
import Data.Word
import Control.Monad
import Data.Maybe(fromJust,isJust)
import Data.Monoid
import Data.Char(chr)
import Test.QuickCheck
import System.Random (Random(..), RandomGen)
import Numeric(readInt)

baseChars = map chr [0..0xFF] 

newtype EntryNr = EN { unNr :: Int } deriving Show
newtype Bytes = BS { unB :: String } deriving Show

instance Arbitrary EntryNr where
    arbitrary = do
      number <- choose (0,3) :: Gen Int
      return (EN (number))

instance Random Word8 where
  randomR = integralRandomR
  random = randomR (minBound, maxBound)

instance Arbitrary Word8 where
  arbitrary = choose (minBound, maxBound)

instance Arbitrary Bytes where
  arbitrary = do 
    perm <- permutation baseChars
    number <- choose (1,(min 255 (length perm))) :: Gen Int
    return (BS (take number perm))
  shrink (BS s) = [BS $ take (length s - 1) s]

instance Arbitrary DiagnosisMessage where
  arbitrary = do
    src <- arbitrary
    target <- arbitrary
    perm <- permutation [0..255]
    let payload = take (min 255 (length perm)) perm
    return $ DiagnosisMessage src target payload
  shrink (DiagnosisMessage s t ps) = 
    if length ps > 1 
      then [DiagnosisMessage s t (take (length ps - 1) ps)]
      else []

instance Arbitrary ControlBit where
  arbitrary = oneof [return DataBit,return AckBit]

integralRandomR (a,b) g = case randomR (c,d) g of
                            (x,h) -> (fromIntegral x, h)
    where (c,d) = (fromIntegral a :: Integer,
                   fromIntegral b :: Integer)

extractOneChar :: Gen [a] -> Gen a
extractOneChar values = do
  xs <- values
  elements xs

vectorOf k gen = sequence [ gen | _ <- [1..k] ]

combination :: [a] -> Gen [a]
combination xs = do
  count <- choose(1,10) :: Gen Int
  sequence [ elements xs | _ <- [1..count]] 
  
permutation :: Eq a => [a] -> Gen [a]
permutation initial = inner initial []
  where
    inner :: Eq a => [a] -> [a] -> Gen [a]
    inner [] accum = return accum
    inner xs accum = do
      p <- choose(0,(length xs)-1)
      inner (delete (xs!!p) xs) ((xs!!p):accum)

prop_bytes2Msg2bytes ::  Bytes -> Bool
prop_bytes2Msg2bytes bytes = bytes2msg2bytes msgBytes == msgBytes
    where msgBytes = ((encodeLength $ length byteList) ++ [0,1]) ++ byteList
          byteList = unB bytes
          bytes2msg2bytes bs = maybe "" msg2ByteString (bytes2msg bs)

prop_diag2hsfz2diag :: DiagnosisMessage -> ControlBit -> Bool
prop_diag2hsfz2diag dm cb = (hsfz2diag . (flip diag2hsfz cb)) dm == dm

