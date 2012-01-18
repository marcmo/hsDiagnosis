module Diagnoser.ScriptDatatypes

where

import Data.Word(Word8,Word16)
import Util.Encoding

data ScriptElement = ScriptTestCase TestCase
                   | Loop String Int [ScriptElement]
                   | Group String [ScriptElement]
                   | Wait  Int
                   | Useraction String
                   | CanMsg String Word16 [Word8]
                   | CyclicCanMsg String Word16 [Word8] Int [ScriptElement]
  deriving (Show,Eq)

data DiagScript = DiagScript {
  scriptElements :: [ScriptElement]
} deriving (Show,Eq)

data TestCase = TestCase {
  caseName :: String,
  sendMsg  :: DiagScriptMsg,
  expected :: ExpectedMsg,
  timeout  :: Int,
  source   :: Maybe Word8,
  target   :: Maybe Word8
} deriving (Show,Eq)

mkTestCase n m e time s t = TestCase n sendM exp time s t
  where sendM = DiagScriptMsg s t m
        exp   = ExpectedMsg t s e

data Match = Match Word8
           | Questioned String        
           | Star 
     deriving (Eq,Show)

data ExpectedPayload  = ExpectedPayload  [[Match]] --Todo: rename
                      | EveryOrNoMsg   -- corresponds to [#]
                      | EveryMsg       -- corresponds to [*]
                      | NoMsg          -- corresponds to [] 
      deriving (Eq,Show)

data ExpectedMsg = ExpectedMsg {
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
                  deriving (Eq)


-- Note: show parameter list now as in specification
instance Show Parameter where 
  show (Parameter name value) = '"' : name ++ "\"=\"" ++ showAsHexString value ++ "\""
  showList cs = showChar '[' . showl cs
              where showl [c]    = shows c . showChar ']'
                    showl (c:cs) = shows c . showChar ';' . showl cs 

a = [Parameter "name" [0x10], Parameter "name2" [0x10]]








