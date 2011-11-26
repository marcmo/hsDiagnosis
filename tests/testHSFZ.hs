import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Word(Word8)
import Data.List
import Control.Applicative((<$>))
import qualified Data.ByteString as S
import Test.QuickCheck hiding ((.&.))
import Com.HSFZMessage
import Com.DiagMessage
import Util.Encoding

main = defaultMain tests

tests = [
        testGroup "hsfz-msg Group" [
                testProperty "serializing" prop_serializeDeserialize,
                testProperty "deserializing" prop_deserializeserialize,
                testProperty "serializing stream" prop_serializeDeserializeStream
            ]
    ]

instance Arbitrary HSFZMessage where
  arbitrary = do
    cbit <- elements [AckBit,DataBit]
    len <- choose (0,100)
    payload <- vectorOf len arbitrary -- :: Word8) 
    return $ HSFZMessage cbit len payload
  shrink (HSFZMessage b len xs) = let half = div len 2 in
    if half == 0 then []
      else [HSFZMessage b (half) (drop (len - half) xs)]

newtype HSFZBytes = MkBytes { bytes :: S.ByteString } deriving (Show)
instance Arbitrary HSFZBytes where
  arbitrary = do
    bs <- listOf (arbitrary :: Gen Word8)
    let lengthBytes = encodeLength $ length bs
    cbit <- control2Int <$> elements [AckBit,DataBit]
    let allBytes = lengthBytes ++ [0,cbit] ++ bs
    return $ MkBytes $ S.pack allBytes
    
prop_deserializeserialize bytes@(MkBytes bs) = 
  bs == eventualBytes
    where (Just hsfzMsg) = deserialize2Hsfz bs
          eventualBytes = msg2ByteString hsfzMsg

prop_serializeDeserialize hsfzMsg = 
  collect (payloadLen hsfzMsg) $
  (Just hsfzMsg) == eventualMsg
    where eventualMsg = deserialize2Hsfz $ msg2ByteString hsfzMsg

prop_serializeDeserializeStream msgs =
  stream == deserialized
    where
      stream = MessageStream msgs
      serialized = serializeFromStream stream
      deserialized = deserialize2HsfzStream serialized

baseChars = [0..0xFF] 

newtype Bytes = BS { unB :: S.ByteString } deriving Show

instance Arbitrary Bytes where
  arbitrary = do 
    perm <- permutation baseChars
    number <- choose (1,(min 255 (length perm))) :: Gen Int
    return (BS (S.pack $ take number perm))
  shrink (BS s) = [BS $ S.take (S.length s - 1) s]

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

extractOneChar :: Gen [a] -> Gen a
extractOneChar values = do
  xs <- values
  elements xs

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
prop_bytes2Msg2bytes bytes = bytes2msg2bytes (S.pack msgBytes) == (S.pack msgBytes)
    where msgBytes = ((encodeLength $ S.length byteList) ++ [0,1]) ++ (S.unpack byteList)
          byteList = unB bytes
          bytes2msg2bytes bs = maybe S.empty msg2ByteString (deserialize2Hsfz bs)

prop_diag2hsfz2diag :: DiagnosisMessage -> ControlBit -> Bool
prop_diag2hsfz2diag dm cb = (hsfz2diag . (flip diag2hsfz cb)) dm == dm

