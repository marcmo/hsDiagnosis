module Diagnoser.TestCaseExecuter

where

import Util.Encoding
import Data.Word
import Com.DiagClient
import Diagnoser.ScriptDatatypes
import Diagnoser.Matcher
import DiagnosticConfig(femConfig,zgwConfig)
import qualified Com.DiagClient     as DC

import Control.Concurrent
import Control.Monad

--conf = zgwConfig "10.40.39.85"
conf = femConfig "localhost"

data TestResult = TR {
  testCount :: Int,
  errors :: [String]
} deriving (Show)

combine :: TestResult -> TestResult -> TestResult
combine (TR a as) (TR b bs) = TR (a+b) (as ++ bs)

expect :: [Word8] -> Maybe DiagnosisMessage -> IO (Maybe String)
expect xs Nothing = return Nothing
expect xs (Just msg) = 
    if diagPayload msg == xs
      then return Nothing
      else return $ Just errorMsg
        where errorMsg = "damn it!! expected" ++ showAsHexString xs ++ " but was " ++ showAsHexString (diagPayload msg)
      
runScript (DiagScript ss) =  runScriptWithIndent 0 ss

runScriptWithIndent indent = mapM_ (runScriptElement indent) 


runScriptElement n s@(ScriptTestCase (TestCase name 
                                               (DiagScriptMsg _ _ sent) 
                                               e@(ExpectedMsg   _ _ (ExpectedPayload expected ))  
                                               timeout source target)) = 
  do writeScriptElement n s
     response <- DC.sendData conf sent
     let m = matches (head response) e
     putStrLn ""
     putIndLn n $ "!! Result: " ++ show m
     if not m then putIndLn n $ "!! Actual response was:  "   ++ show response          
     else putStr ""
     putStrLn ""

runScriptElement n s@(Loop name count ss) = do writeScriptElementStart n s
                                               replicateM_ count (runScriptWithIndent (n + 2) ss)
                                               writeScriptElementEnd n s

runScriptElement n s@(Group name ss)      = do writeScriptElementStart n s
                                               runScriptWithIndent (n + 2)  ss
                                               writeScriptElementEnd n s  
runScriptElement n s@(CyclicCanMsg _ _ _ _ ss) = do writeScriptElementStart n s >> putStrLn "!! CanMsg not Implemented!!!!!!!"
                                                    runScriptWithIndent (n + 2)  ss  
                                                    writeScriptElementEnd n s
runScriptElement n s@(CanMsg name id dat)     = writeScriptElement n s >> putStrLn "!! CanMsg not Implemented!!!!!!!"
runScriptElement n s@(Wait time)              = writeScriptElement n s >> threadDelayMs time
runScriptElement n (Useraction msg)           = putIndLn n $ "USERACTION " ++ "(" ++ quoted msg ++")"



quoted  s = "\"" ++ s ++ "\""
bracketed s =  "[" ++ s ++ "]"
threadDelayMs t = threadDelay (1000 * t)
nSpaces n = replicate n ' '
putIndLn n s = putStrLn $ nSpaces n ++ s



writeScriptElementStart n (Group name ss)      = putIndLn n $ "GROUPSTART " ++ bracketed name
writeScriptElementStart n (Loop name count ss) = putIndLn n $ "LOOPSTART  " ++ bracketed name ++ " COUNT " ++ bracketed (show count)
writeScriptElementStart n (CyclicCanMsg name id dat time ss) = putIndLn n $ "STARTCYCLICCANMSG "

writeScriptElementEnd n (Group name  ss)     = putIndLn n $ "GROUPEND "   ++ bracketed name
writeScriptElementEnd n (Loop name count ss) = putIndLn n $ "LOOPEND "    ++ bracketed name
writeScriptElementEnd n (CyclicCanMsg name id dat time ss) = putIndLn n $ "STOPCYCLICCANMSG " ++ bracketed name ++
                                                                          " ID "              ++ bracketed (show id) ++ 
                                                                          " DATA "            ++ bracketed (show dat)  ++
                                                                          " CYCLE "           ++ bracketed (show time)  


writeScriptElement n (CanMsg name id dat)     = putIndLn n $ "CANMSG " ++ bracketed name         ++ 
                                                             " ID "    ++ (bracketed . show) id  ++ 
                                                             " DATA "  ++ (bracketed . show) dat
writeScriptElement n (Wait time)              = putIndLn n $ "WAIT "       ++ bracketed (show time)
writeScriptElement n (ScriptTestCase (TestCase name 
                                             (DiagScriptMsg _ _ sent) 
                                             (ExpectedMsg   _ _ (ExpectedPayload expected ))  
                                              timeout source target)) = 
  putIndLn n $  "DIAG "    ++ bracketed name              ++ 
               " SEND "    ++ (bracketed . show) sent     ++ 
               " EXPECT "  ++ (bracketed . show) expected ++ 
               " TIMEOUT " ++ (bracketed . show) timeout  ++ sourceAndTarget source  target
  where sourceAndTarget Nothing Nothing = ""
        sourceAndTarget source  target  = " SOURCE "  ++ (bracketed . show) source   ++ 
                                          " TARGET "  ++ (bracketed . show) target