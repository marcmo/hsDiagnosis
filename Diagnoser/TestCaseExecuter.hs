module Diagnoser.TestCaseExecuter

where

import Util.Encoding
import Data.Word
import Com.DiagClient
import Diagnoser.ScriptDatatypes

data TestResult = TR {
  testCount :: Int,
  errors :: [String]
} deriving (Show)

combine :: TestResult -> TestResult -> TestResult
combine (TR a as) (TR b bs) = TR (a+b) (as ++ bs)

expect :: [Word8] -> Maybe DiagnosisMessage -> IO (Maybe String)
expect xs Nothing = return Nothing
expect xs (Just msg) = 
    if (diagPayload msg == xs) 
      then return Nothing
      else return $ (Just errorMsg)
        where errorMsg = "damn it!! expected" ++ showAsHexString xs ++ " but was " ++ (showAsHexString $ diagPayload msg)
      
runScript :: DiagScript -> IO ()
runScript (DiagScript ss) = do
  mapM_ runScriptElement ss

runScriptElement (Wait n) = print $ "waiting for " ++ show n

-- runTestCase ::  TestCase -> IO (Maybe ErrorDesc)
-- runTestCase (TestCase name msg expected timeout) =
--   sendData conf (diagPayload msg) >>= expect expected 
-- 
-- runTestRun :: TestRun -> IO TestResult
-- runTestRun (SingleLevel ts) = do
--   mes <- forM ts runTestCase
--   return $ TR (length mes) (catMaybes mes)
-- runTestRun (MultiLevel rs) = do
--   foldM comb (TR 0 []) rs 
--     where comb acc tr =  (combine acc) `liftM` (runTestRun tr)

