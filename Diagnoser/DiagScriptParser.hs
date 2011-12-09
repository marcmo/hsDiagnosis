module Diagnoser.DiagScriptParser

where 

import qualified Text.ParserCombinators.Parsec.Token as P
import Util.Encoding
import Text.ParserCombinators.Parsec.Language
import Com.DiagMessage
import Data.Word(Word8,Word16)
import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Data.Char
import Data.Maybe

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
  sendMsg  :: DiagnosisMessageMaybe,
  expected :: ExpectedMessage,
  timeout  :: Int,
  source   :: Maybe Word8,
  target   :: Maybe Word8
} deriving (Show,Eq)


type ParaName = String
type ParaValue = [Word8]
--type ParaValue = String
data Parameter = Parameter ParaName ParaValue
                  deriving (Show,Eq)


-- data CanMsg = CanMsg {
--   name    :: String,
--   id      :: Word16,
--   dataMsg :: DiagnosisMessage}deriving (Show,Eq)

--data CanMsg = CanMsg String Word16 [Word8]
--              deriving (Show,Eq)



mkTestCase n m e time s t = TestCase n sendM exp time s t
  where sendM = DiagnosisMessageMaybe s t m
        exp   = ExpectedMessage t s e


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
paramVarChars = ['a'..'z']++['A'..'Z']++"_,- "++['0'..'9']

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
         <|> do reserved "CALLSCRIPT"
                path <- filePath
                whiteSpace
                pL <- option [] parameterList
                return $ Callscript path pL
         <|> do reserved "CANMSG"
                n <- nameInBrackets
                reserved "ID"
                id <- brackets hexNum16
                reserved "DATA"
                dat <- hexList
                return $ CanMsg n id dat      
         <|> do reserved "STARTCYCLICCANMSG"
                n   <- nameInBrackets
                reserved "ID"
                id  <- brackets hexNum16 
                reserved "DATA"
                dat <- hexList
                reserved "CYCLE"
                num <- brackets (many1 digit)
                ss  <- many scriptelem
                reserved "STOPCYCLICCANMSG"
                brackets (string n)
                return $ CyclicCanMsg n id dat (read num) ss
         <|> ScriptTestCase <$> testcase
         <?> "scriptelement"



-- TODO: make filePath match windows/unix file paths
filePath ::  CharParser () String
filePath = (many1 $ noneOf "\"\r\n ")

parameter ::  GenParser Char () Parameter
parameter = do char '"'
               name <- (many1 $ oneOf namechars)
               char '"'; char '=';  char '"'
               var <- hexListNoBrackets
               char '"'
               return $ Parameter name var


parameterList ::  CharParser () [Parameter]
parameterList = brackets $ (sepBy parameter (symbol ";"))

hexListNoBrackets ::  CharParser () [Word8]
hexListNoBrackets = (sepBy hexNum (symbol ","))

                
testcase :: Parser TestCase
testcase =
   mkTestCase <$> (reserved "DIAG" *> nameInBrackets)
              <*> (reserved "SEND" *> hexList)
              <*> (reserved "EXPECT" *> expectedMsg)
              <*> (reserved "TIMEOUT" *> read `fmap` brackets (many1 digit))
              <*> ((option () (reserved "SOURCE")) *> (do a <- option 0 (brackets hexNum)
                                                          return $ case a of {(0) -> Nothing; otherwise -> Just a}))
              <*> ((option () (reserved "TARGET")) *> (do a <- option 0 (brackets hexNum)
                                                          return $ case a of {(0) -> Nothing; otherwise -> Just a}))


hexNumMatch ::  GenParser Char () Match
hexNumMatch = do s <- many1 (oneOf (['0'..'9']++['a'..'f']++['A'..'F']))
                 return $ Match (string2hex s)



match ::  GenParser Char () Match
match = do s <- try (char '*')
           return $ Star 
       <|> do s <- try (count 2 (oneOf (['0'..'9']++['a'..'f']++['A'..'F'])))
    --        s <- try (many1 (oneOf (['0'..'9']++['a'..'f']++['A'..'F'])))
              return $ Match (string2hex s)
       <|> do s <- try (count 2 (oneOf (['0'..'9']++['a'..'f']++['A'..'F']++['?'])))
              return $ Questioned (map toUpper $ s)
       <|> do s <- (oneOf (['0'..'9']++['a'..'f']++['A'..'F']))       
              return $ Match (string2hex [s])
       <?> "match" 

matches ::  CharParser () [Match]
matches = (sepBy match (symbol ","))


expectedMsg :: GenParser Char () ExpectedMsg
expectedMsg =  do try (brackets $ (string ""))
                  return $ NoMsg
           <|> do try (brackets $ (char '*'))
                  return $ EveryMsg
           <|> do try (brackets $ (char '#'))
                  return $ EveryOrNoMsg
           <|> do ret <- try (brackets matches)
                  return $ ExpectedMsg [ret]



hexList ::  CharParser () [Word8]
hexList = brackets $ (sepBy hexNum (symbol ","))



-- TODO: check if a native Parsec function exits for this
parseString :: GenParser Char st String
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return  x


hexNum ::  GenParser Char st Word8
hexNum = do s <- many1 (oneOf (['0'..'9']++['a'..'f']++['A'..'F']))
            return $ string2hex s

hexNum16 ::  GenParser Char st Word16
hexNum16 = do s <- many1 (oneOf (['0'..'9']++['a'..'f']++['A'..'F']))
              return $ string2hex16 s


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

