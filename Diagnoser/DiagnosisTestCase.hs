module Diagnoser.DiagnosisTestCase

where

import Com.DiagMessage
import Data.Word
import Util.Encoding

data TestCase = TestCase {
  name :: String,
  msg2send :: DiagnosisMessage,
  expectedPayload :: ExpectedMsg,
  timeoutMs :: Int
}
data TestRun = SingleLevel [TestCase] |
               MultiLevel [TestRun]

instance Show TestCase where
  show (TestCase n m xs t) =
    n ++ ": send: " ++ (show m) ++ ", expected:" ++ show xs ++ ", timeout:" ++ show t
--  n ++ ": send: " ++ (show m) ++ ", expected:" ++ showAsHexString xs ++ ", timeout:" ++ show t



