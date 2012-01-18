module Diagnoser.Matcher where

import Data.Word
import Diagnoser.ScriptDatatypes
import Numeric
import Data.Char
import Com.DiagMessage
import Data.List

-- TODO: improve Questioned stuff
matchQuestioned :: String -> String -> Bool
matchQuestioned x         "??"         = True
matchQuestioned (x:y:[]) ('?': z :[])  = y == toUpper z
matchQuestioned (x:y:[]) ( z :'?':[])  = x == toUpper z
matchQuestioned _ _                    = False 

hexStr x = let hexS   = map toUpper (showHex x "") in
               if length hexS == 1 
               then '0' : hexS
               else hexS

matcher1 :: [Word8] -> [Match] -> Bool
matcher1   []    []      = True
matcher1   xs    []      = False
matcher1   []    ys      = False
matcher1   (x:xs) (Star         :ys)                                =  any (`matcher1` ys) (tails (x : xs))
matcher1   (x:xs) (Questioned y :ys) | matchQuestioned (hexStr x) y = matcher1 xs ys
matcher1   (x:xs) (Match y      :ys) | x ==  y                      = matcher1 xs ys
matcher1   x      y      = False                      

-- TODO: implement NoMsg (with timings)
matcher ::  [Word8] -> ExpectedPayload -> Bool
matcher _ EveryMsg     = True  
matcher _ EveryOrNoMsg = True  
matcher _ NoMsg        = undefined
matcher response (ExpectedPayload expected) = any (matcher1 response) expected

matches :: DiagnosisMessage -> ExpectedMsg -> Bool
matches (DiagnosisMessage _ _ response) (ExpectedMsg _ _ expected) = matcher response expected

