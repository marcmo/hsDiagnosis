module Diagnoser.Matcher (matches,matcher,matcher1,matchQuestioned)

where

import  Com.DiagMessage
import  Data.Char
import  Data.List
import  Data.Word
import  Diagnoser.ScriptDatatypes
import  Numeric

-- TODO: improve Questioned stuff
matchQuestioned :: String -> String -> Bool
matchQuestioned _ "??" = True
matchQuestioned (_:y:[]) ('?': z :[]) = y == toUpper z
matchQuestioned (x:_:[]) ( z :'?':[]) = x == toUpper z
matchQuestioned _ _ = False

hexStr x = let hexS   = map toUpper (showHex x "") in
               if length hexS == 1
               then '0' : hexS
               else hexS

matcher1 :: [Word8] -> [Match] -> Bool
matcher1 [] [] = True
matcher1 _ [] = False
matcher1 [] _ = False
matcher1 (x:xs) (Star:ys) =  any (`matcher1` ys) (tails (x : xs))
matcher1 (x:xs) (Questioned y:ys) | matchQuestioned (hexStr x) y = matcher1 xs ys
matcher1 (x:xs) (Match y:ys) | x ==  y = matcher1 xs ys
matcher1 _  _ = False

-- TODO: implement NoMsg (with timings)
matcher ::  [Word8] -> ExpectedPayload -> Bool
matcher _ EveryMsg = True
matcher _ EveryOrNoMsg = True
matcher _ NoMsg = undefined
matcher response (ExpectedPayload expect) = any (matcher1 response) expect

matches :: DiagnosisMessage -> ExpectedMsg -> Bool
matches (DiagnosisMessage _ _ response) (ExpectedMsg _ _ expect) = matcher response expect

