import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Word(Word8)
import Util.Encoding(int2Word8)
import Control.Applicative((<$>))
import qualified Data.ByteString as S
import Data.Bits
import Test.QuickCheck hiding ((.&.))
import Test.HUnit
import Com.HSFZMessage
import Debug.Trace

main = defaultMain tests

tests = [
        testGroup "hsfz-msg Group" [
                testProperty "serializing" prop_serializeDeserialize,
                testProperty "deserializing" prop_deserializeserialize
            ]
    ]

instance Arbitrary HSFZMessage where
  arbitrary = do
    cbit <- elements [AckBit,DataBit]
    len <- choose (0,100)
    payload <- vectorOf len arbitrary -- :: Word8) 
    return $ HSFZMessage cbit len payload

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
    where (Just hsfzMsg) = bytes2msg bs
          eventualBytes = msg2ByteString hsfzMsg

prop_serializeDeserialize hsfzMsg = 
  collect (payloadLen hsfzMsg) $
  (Just hsfzMsg) == eventualMsg
    where eventualMsg = bytes2msg $ msg2ByteString hsfzMsg

encodeLength :: Int -> [Word8]
encodeLength len =
            [0xFF .&. int2Word8 (len `shiftR` 24)
            ,0xFF .&. int2Word8 (len `shiftR` 16)
            ,0xFF .&. int2Word8 (len `shiftR` 8)
            ,0xFF .&. int2Word8 (len `shiftR` 0)]

