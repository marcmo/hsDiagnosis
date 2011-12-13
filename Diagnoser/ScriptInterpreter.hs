module Diagnoser.ScriptInterpreter

where

import Data.Word(Word8,Word16)


--import Diagnoser.DiagScriptParser
--import Diagnoser.TestCaseExecuter
--import Diagnoser.DiagnosisTestCase
import Control.Monad




data ScriptElement = ScriptTestCase TestCase
                   | Loop String Int [ScriptElement]
                   | Group String [ScriptElement]
                   | Wait  Int
                   | Useraction String
                   | Callscript FilePath [Parameter]
                   | CanMsg String Word16 [Word8]
                   | CyclicCanMsg String Word16 [Word8] Int [ScriptElement]
  deriving (Show,Eq)
data DiagScript = DiagScript {
  scriptElements :: [ScriptElement]
} deriving (Show,Eq)

data TestCase = TestCase {
  caseName :: String,
  sendMsg  :: DiagScriptMsg,
  expected :: ExpectedMessage,
  timeout  :: Int,
  source   :: Maybe Word8,
  target   :: Maybe Word8
} deriving (Show,Eq)


mkTestCase n m e time s t = TestCase n sendM exp time s t
  where sendM = DiagScriptMsg s t m
        exp   = ExpectedMessage t s e




data Match = Match Word8
           | Questioned String        
           | Star 
     deriving (Eq,Show)


data ExpectedPayload  = ExpectedMsg  [[Match]]
                      | EveryOrNoMsg   -- corresponds to [#]
                      | EveryMsg       -- corresponds to [*]
                      | NoMsg          -- corresponds to [] 
      deriving (Eq,Show)


data ExpectedMessage = ExpectedMessage {
  expectSource :: Maybe Word8,  -- ??? switch source and target ???
  expectTarget :: Maybe Word8,
  expectPayload :: ExpectedPayload
} deriving (Eq,Show)



data DiagScriptMsg = DiagScriptMsg {
  diagSourceM  :: Maybe Word8,
  diagTargetM  :: Maybe Word8,
  diagPayloadM :: [Word8]

} deriving (Eq,Show)



type ParaName = String
type ParaValue = [Word8]
data Parameter = Parameter ParaName ParaValue
                  deriving (Show,Eq)







-- data UserAction = UA {
--   userMessage :: String
-- }
-- data DiagScript = TestCase |
--                   Wait Integer |
--                   Callscript FilePath |
--                   Useraction
--                   -- Groupstart
--                   -- Groupend
--                   -- Loopstart
--                   -- Loopend

-- runDiagScript = do
--   let f = "diagnoserscript.skr"
--   inpStr <- readFile f
--   case parseInput inpStr of 
--     Left err -> do  putStrLn "Error parsing input:"
--                     print err
--     Right r -> do res <- runTestRun r
--                   print res

