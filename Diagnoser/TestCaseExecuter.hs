module Diagnoser.TestCaseExecuter

where

import Util.Encoding
import Data.Word
import Com.DiagClient
import Diagnoser.ScriptDatatypes
import Diagnoser.Matcher

import Control.Monad
import Control.Concurrent 

import DiagnosticConfig(femConfig)
import qualified Com.DiagClient     as DC


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
      
-- runScript :: DiagScript -> IO ()
-- runScript (DiagScript ss) = do
--   mapM_ runScriptElement ss

runScript (DiagScript ss) = do
  runScriptWithIndent 0 ss

runScriptWithIndent indent  ss = do
  mapM_ (runScriptElement indent)   ss



-------------------------------------------------------------------------------------------------------

runScriptElement n s@(ScriptTestCase (TestCase name 
                                               (DiagScriptMsg _ _ sent) 
                                               e@(ExpectedMsg   _ _ (ExpectedPayload expected ))  
                                               timeout source target)) = 
  do writeScriptElement n s
     response <- DC.sendData (femConfig "localhost") sent
     putStrLn $ "Test Result: " ++ (show $ matches (head response) e)


runScriptElement n (Loop name count ss) = do putStrLn $ nSpaces n ++ "LOOP "  ++ quoted name  ++ " started"
                                             rep count
                                          where rep 0 = putStrLn $ nSpaces n ++ "LOOP "  ++ quoted name ++ " ended"
                                                rep i = do runScriptWithIndent (n + 2) ss
                                                           rep (i -1 )

runScriptElement n (Group name ss)      = do putStrLn $ nSpaces n ++ "GROUP " ++ quoted name ++ " started"
                                             runScriptWithIndent (n + 2)  ss
                                             putStrLn $ nSpaces n ++ "GROUP " ++ quoted name ++ " ended"
                                                  

runScriptElement n (CyclicCanMsg _ _ _ _ _) = putStrLn $ nSpaces n ++ "CYCLICCANMSG: "

runScriptElement n (Callscript file params) = putStrLn $ nSpaces n ++ "CALLSCRIPT of file: " ++ quoted file ++ "  with parameters   " ++ (quoted . show)  params

runScriptElement n (Wait time)              = putStrLn $ nSpaces n ++ "WAITing for " ++ show time  ++ " ms"


runScriptElement n (Useraction msg)         = putStrLn $ nSpaces n ++ "USERACTION " ++ quoted msg

runScriptElement n (CanMsg name id dat)     = putStrLn $ "sending CANMSG " ++ quoted name         ++ 
                                                         " with ID "       ++ (quoted . show) id  ++ 
                                                         " and DATA "      ++ (quoted . show) dat


-------------------------------------------------------------------------------------------------------



nSpaces n = take n (repeat ' ')
quoted  s = "\"" ++ s ++ "\""



writeScriptElement n (ScriptTestCase (TestCase name 
                                             (DiagScriptMsg _ _ sent) 
                                             (ExpectedMsg   _ _ (ExpectedPayload expected ))  
                                              timeout source target)) = 
  putStrLn $ "sending TestCase " ++ quoted name              ++ 
             " with message "    ++ (quoted . show) sent     ++ 
             " and expected msg" ++ (quoted . show) expected ++ 
             " with a timout "   ++ (quoted . show) timeout  ++ 
             " from source "     ++ (quoted . show) source   ++ 
             " to target "       ++ (quoted . show) target




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

