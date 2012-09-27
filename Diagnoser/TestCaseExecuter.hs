module Diagnoser.TestCaseExecuter
      (
         runScript
        ,TestResult(..)
        ,combine
        ,expect
      )
where

import Util.Encoding
import Data.Word
import Com.DiagClient
import Diagnoser.ScriptDatatypes
import Diagnoser.Matcher
import DiagnosticConfig(femConfig)
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
expect _ Nothing = return Nothing
expect xs (Just msg) =
    if diagPayload msg == xs
      then return Nothing
      else return $ Just errorMsg
        where errorMsg = "damn it!! expected" ++ showAsHexString xs ++ " but was " ++ showAsHexString (diagPayload msg)

runScript ::  DiagScript -> IO ()
runScript (DiagScript ss) =  runScriptWithIndent 0 ss

runScriptWithIndent ::  Int -> [ScriptElement] -> IO ()
runScriptWithIndent indent = mapM_ (runScriptElement indent)

runScriptElement ::  Int -> ScriptElement -> IO ()
runScriptElement n s@(ScriptTestCase
                      (TestCase _
                          (DiagScriptMsg _ _ sent)
                          e@(ExpectedMsg   _ _ (ExpectedPayload _ ))
                          _ _ _)) = do
        writeScriptElement n s
        response <- DC.sendData conf sent
        let m = matches (head response) e
        putStrLn ""
        putIndLn n $ "!! Result: " ++ show m
        if not m then putIndLn n $ "!! Actual response was:  "   ++ show response
          else putStr ""
        putStrLn ""
runScriptElement n s@(Loop _ count ss) = do
        writeScriptElementStart n s
        replicateM_ count (runScriptWithIndent (n + 2) ss)
        writeScriptElementEnd n s
runScriptElement n s@(Group _ ss) = do
        writeScriptElementStart n s
        runScriptWithIndent (n + 2)  ss
        writeScriptElementEnd n s
runScriptElement n s@(CyclicCanMsg _ _ _ _ ss) = do
        writeScriptElementStart n s >> putStrLn "!! CanMsg not Implemented!!!!!!!"
        runScriptWithIndent (n + 2)  ss
        writeScriptElementEnd n s
runScriptElement n s@CanMsg {} = writeScriptElement n s >> putStrLn "!! CanMsg not Implemented!!!!!!!"
runScriptElement n s@(Wait time) = writeScriptElement n s >> threadDelayMs time
runScriptElement n (Useraction msg) = putIndLn n $ "USERACTION " ++ "(" ++ quoted msg ++")"
runScriptElement _ e = error $ "script element " ++ show e ++ " not supported"

quoted  s = "\"" ++ s ++ "\""
bracketed s =  "[" ++ s ++ "]"
threadDelayMs t = threadDelay (1000 * t)
nSpaces n = replicate n ' '
putIndLn n s = putStrLn $ nSpaces n ++ s

writeScriptElementStart ::  Int -> ScriptElement -> IO ()
writeScriptElementStart n (Group name _) = putIndLn n $ "GROUPSTART " ++ bracketed name
writeScriptElementStart n (Loop name count _) = putIndLn n $ "LOOPSTART  " ++ bracketed name ++ " COUNT " ++ bracketed (show count)
writeScriptElementStart n CyclicCanMsg{} = putIndLn n "STARTCYCLICCANMSG "
writeScriptElementStart _ m = error $ "unsupported script element start: " ++ show m

writeScriptElementEnd ::  Int -> ScriptElement -> IO ()
writeScriptElementEnd n (Group name _)     = putIndLn n $ "GROUPEND "   ++ bracketed name
writeScriptElementEnd n (Loop name _ _) = putIndLn n $ "LOOPEND "    ++ bracketed name
writeScriptElementEnd n (CyclicCanMsg name canid dat time _) = putIndLn n $ "STOPCYCLICCANMSG " ++ bracketed name ++
                                                                          " ID "              ++ bracketed (show canid) ++
                                                                          " DATA "            ++ bracketed (show dat)  ++
                                                                          " CYCLE "           ++ bracketed (show time)
writeScriptElementEnd _ m = error $ "unsupported script element end: " ++ show m


writeScriptElement ::  Int -> ScriptElement -> IO ()
writeScriptElement n (CanMsg name canId dat)     = putIndLn n $ "CANMSG " ++ bracketed name         ++
                                                             " ID "    ++ (bracketed . show) canId  ++
                                                             " DATA "  ++ (bracketed . show) dat
writeScriptElement n (Wait time)              = putIndLn n $ "WAIT "       ++ bracketed (show time)
writeScriptElement n (ScriptTestCase (TestCase name
                                             (DiagScriptMsg _ _ sent)
                                             (ExpectedMsg   _ _ (ExpectedPayload e ))
                                              time src tgt)) =
  putIndLn n $  "DIAG "    ++ bracketed name              ++
               " SEND "    ++ (bracketed . show) sent     ++
               " EXPECT "  ++ (bracketed . show) e ++
               " TIMEOUT " ++ (bracketed . show) time  ++ sourceAndTarget src tgt
  where sourceAndTarget Nothing Nothing = ""
        sourceAndTarget s t = " SOURCE "  ++ (bracketed . show) s   ++
                                          " TARGET "  ++ (bracketed . show) t
writeScriptElement _ m = error $ "unsupported script element: " ++ show m

