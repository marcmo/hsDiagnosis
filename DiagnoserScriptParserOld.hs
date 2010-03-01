module DiagnoserScriptParser

where

import ParserUtil
import DiagMessage
import DiagnosisTestCase
import Util
import Data.Word
import Monad(liftM)
import System(getArgs)

baseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(), "

parseInput ::  [Char] -> Either ParseError TestRun
parseInput = parse p_diagnoserFile "(unknown)"

p_diagnoserFile ::  GenParser Char () TestRun
p_diagnoserFile = do
  lines <- endBy p_line eol
  return $ SingleLevel (concat lines)

p_line = (try p_comment) <|> try loopstart <|> try loopend <|> p_testCase
loopstart = string "LOOPSTART" *> (many (noneOf "\n")) *> return []
loopend = string "LOOPEND" *> (many (noneOf "\n")) *> return []
p_comment = spaces *> (string "//") *> (many (noneOf "\n"))  *> return []

-- DIAG [WRITE_BLOCK_AND_READ_AGAIN_PLAIN] SEND [bf,10,01,0] EXPECT [FF,10,01] TIMEOUT [2000] SOURCE [F4] TARGET [40]
p_testCase :: CharParser () [TestCase]
p_testCase = do
  name <- spaces *> string "DIAG" *> spaces *> char '[' *> many1 (oneOf baseChars) <* char ']'
  toSend <- p_hexRow "SEND"
  toExpect <- p_hexRow "EXPECT"
  timeout <- spaces *> string "TIMEOUT" *> spaces *> char '[' *> many1 digit <* char ']'
  (source:_) <- p_hexRow "SOURCE"
  (target:_) <- p_hexRow "TARGET"
  return $ [TestCase name (DiagnosisMessage source target toSend) toExpect (read timeout)]
  
p_hexRow :: String -> CharParser () [Word8]
p_hexRow label = do
  xs <- spaces *> string label *> spaces *> char '[' *> ((many1 hexDigit) `sepBy` (char ',')) <* char ']'
  return $ map string2hex xs
  
process :: [TestCase] -> String
process tc = show tc 

