module Diagnoser.DiagScriptParser

where 

import qualified Text.ParserCombinators.Parsec.Token as P
import Util.Encoding
import Text.ParserCombinators.Parsec.Language
import Com.DiagMessage
import Data.Word(Word8)
import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

data ScriptElement = ScriptTestCase TestCase
                   | Loop String Int [ScriptElement]
                   | Group String [ScriptElement]
                   | Wait  Int
                   | Useraction String
  deriving (Show,Eq)
data DiagScript = DiagScript {
  scriptElements :: [ScriptElement]
} deriving (Show,Eq)
data TestCase = TestCase {
  caseName :: String,
  sendMsg  :: DiagnosisMessage,
  expected :: DiagnosisMessage,
  timeout  :: Int,
  source   :: Word8,
  target   :: Word8
} deriving (Show,Eq)
mkTestCase n m e time s t = TestCase n sendM exp time s t
  where sendM = DiagnosisMessage s t m
        exp = DiagnosisMessage t s e


lexer :: P.TokenParser ()
lexer = P.makeTokenParser $ haskellStyle
           {  P.reservedNames = ["LOOPSTART", "LOOPEND","GROUPSTART","GROUPEND","DIAG","SEND","EXPECT","TIMEOUT","SOURCE","TARGET"]
            , P.commentLine = "//"
           }   

whiteSpace = P.whiteSpace lexer
symbol     = P.symbol lexer
natural    = P.natural lexer
parens     = P.parens lexer
semi       = P.semi lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
brackets   = P.brackets lexer
reservedOp = P.reservedOp lexer

-- diagscript ::= scriptelem*
-- scriptelem ::= loop | group | test
-- loop       ::= loopstart test* loopend
-- loopstart  ::= "LOOPSTART" name "COUNT" number
-- loopend    ::= "LOOPEND" name
-- test       ::= testname sendmsg expect timeout source target
namechars = ['a'..'z']++['A'..'Z']++"_,- "++['0'..'9']
nameInBrackets = brackets (many1 $ oneOf namechars)
diagscript :: Parser DiagScript
diagscript = do
    whiteSpace
    DiagScript <$> many1 scriptelem <* eof

scriptelem :: Parser ScriptElement
scriptelem = do reserved "LOOPSTART"
                n <- nameInBrackets
                reserved "COUNT"
                num <- brackets (many1 digit)
                ss <- many scriptelem
                reserved "LOOPEND"
                brackets (string n)
                return $ Loop n (read num) ss
         <|> do reserved "GROUPSTART"
                n <- nameInBrackets
                ss <- many1 scriptelem
                reserved "GROUPEND"
                brackets (string n)
                return $ Group n ss
         <|> do reserved "WAIT"
                num <- brackets (many1 digit)
                return $ Wait (read num)
         <|> do reserved "USERACTION"
                txt <- parens parseString
                return $ Useraction txt
         <|> ScriptTestCase <$> testcase
         <?> "scriptelement"
                
testcase :: Parser TestCase
testcase =
   mkTestCase <$> (reserved "DIAG" *> nameInBrackets)
              <*> (reserved "SEND" *> hexList)
              <*> (reserved "EXPECT" *> hexList)
              <*> (reserved "TIMEOUT" *> read `fmap` brackets (many1 digit))
              <*> (reserved "SOURCE" *> brackets hexNum)
              <*> (reserved "TARGET" *> brackets hexNum)

hexList ::  CharParser () [Word8]
hexList = brackets $ (sepBy hexNum (symbol ","))


--parseString :: Stream s m Char => ParsecT s u m LispVal
-- TODO: check if a native Parsec function exits for this
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return  x

hexNum ::  GenParser Char st Word8
hexNum = do s <- many1 (oneOf (['0'..'9']++['a'..'f']++['A'..'F']))
            return $ string2hex s

run :: Show a => Parser a -> String -> IO ()
run p input =
    case (parse p "" input) of
        Left err -> do putStr "parse error at "
                       print err
        Right x  -> print x

runLex :: Show a => Parser a -> String -> IO ()
runLex p input
    = run (do  whiteSpace
               x <-p
               eof
               return x
          ) input

main2 = do 
  contents <- getContents
  runLex diagscript contents

parseScript ::  String -> Either ParseError DiagScript
parseScript = parse diagscript "(unknown)"

