module DiagnosisTestCase

where

import DiagMessage
import Data.Word
import Util

data TestCase = TestCase {
  name :: String,
  msg2send :: DiagnosisMessage,
  expectedPayload :: [Word8],
  timeoutMs :: Int
}
data TestRun = SingleLevel [TestCase] |
               MultiLevel [TestRun]

instance Show TestCase where
  show (TestCase n m xs t) =
    n ++ ": send: " ++ (show m) ++ ", expected:" ++ showAsHexString xs ++ ", timeout:" ++ show t



