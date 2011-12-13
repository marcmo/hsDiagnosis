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


type Expected = [[Match]]

data ExpectedPayload  = ExpectedMsg  Expected
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


-- TODO: add remaning reserved names
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

namechars = ['a'..'z']++['A'..'Z']++"_- "++['0'..'9']


nameInBrackets = brackets (many1 $ oneOf namechars)
diagscript :: Parser DiagScript
diagscript = do
    whiteSpace
    DiagScript <$> many1 scriptelem <* eof

scriptelem :: Parser ScriptElement
scriptelem = ScriptTestCase <$> testcase
         <|> loop
         <|> group
         <|> cyclicCanMsg     
         <|> Useraction <$> (reserved "USERACTION" *> parens parseString)
         <|> Callscript <$> (reserved "CALLSCRIPT" *> filePath)
                        <*> (whiteSpace            *> option [] parameterList)
         <|> CanMsg     <$> (reserved "CANMSG"     *> nameInBrackets)         
                        <*> (reserved "ID"         *> brackets hexNum16)         
                        <*> (reserved "DATA"       *> hexList)
         <|> Wait       <$> fmap read
                            (reserved "WAIT"       *> brackets (many1 digit))
         <?> "scriptelement"
         


loop = do name  <- reserved "LOOPSTART" *>  nameInBrackets
          count <- reserved "COUNT"     *> brackets (many1 digit)
          ss    <- many scriptelem <* 
                   reserved "LOOPEND"    <* brackets (string name) 
          return $ Loop name (read count) ss


group =  do name <- reserved "GROUPSTART" *> nameInBrackets
            ss   <- many1 scriptelem      <*  
                    reserved "GROUPEND"   <* brackets (string name)
            return $ Group name ss


cyclicCanMsg = do name  <- reserved "STARTCYCLICCANMSG" *> nameInBrackets
                  id    <- reserved "ID"                *> brackets hexNum16
                  dat   <- reserved "DATA"              *> hexList
                  cycle <- reserved "CYCLE"             *> brackets (many1 digit)
                  ss    <- many scriptelem              <* 
                           reserved "STOPCYCLICCANMSG"  <* brackets (string name)
                  return $ CyclicCanMsg name id dat (read cycle) ss
                    

testcase :: Parser TestCase
testcase = do name    <- reserved "DIAG"    *> nameInBrackets
              send    <- reserved "SEND"    *> hexList
              expect  <- reserved "EXPECT"  *> expectedMsg
              timeout <- reserved "TIMEOUT" *> brackets (many1 digit)
              snt     <- sourceAndTarget
              return $ uncurry (mkTestCase name send expect (read timeout)) snt
    where sourceAndTarget :: CharParser () (Maybe Word8, Maybe Word8)
          sourceAndTarget = do source <- reserved "SOURCE" *> brackets hexNum
                               target <- reserved "TARGET" *> brackets hexNum
                               return (Just source, Just target)
                         <|> return (Nothing, Nothing)                
      



-- TODO: make filePath match windows/unix file paths
filePath :: CharParser () FilePath
filePath = many1 $ noneOf "\"\r\n "

-- TODO: maybe making parser accept whitespaces around equals sign
parameter ::  GenParser Char () Parameter
parameter  = do char '"'
                name <- many1 $ oneOf namechars
                char '"'; char '=';  char '"'
                var <- hexListNoBrackets
                char '"'
                return $ Parameter name var


parameterList ::  CharParser () [Parameter]
parameterList = brackets $ sepBy parameter (symbol ";")

hexListNoBrackets ::  CharParser () [Word8]
hexListNoBrackets = sepBy hexNum (symbol ",")


match ::  GenParser Char () Match
match = do s <- try (char '*')
           return Star 
       <|> do s <- try (count 2 (oneOf (['0'..'9']++['a'..'f']++['A'..'F'])))
    --        s <- try (many1 (oneOf (['0'..'9']++['a'..'f']++['A'..'F'])))
              return $ Match (string2hex s)
       <|> do s <- try (count 2 (oneOf (['0'..'9']++['a'..'f']++['A'..'F']++ "?")))
              return $ Questioned (map toUpper s)
       <|> do s <- oneOf (['0'..'9']++['a'..'f']++['A'..'F'])
              return $ Match (string2hex [s])
       <?> "match" 

matches ::  CharParser () [Match]
matches =  sepBy match (symbol ",")


expectedMsg :: GenParser Char () ExpectedPayload
expectedMsg =  do try (brackets $ string "")
                  return NoMsg
           <|> do try (brackets $ char '*')
                  return EveryMsg
           <|> do try (brackets $ char '#')
                  return EveryOrNoMsg
           <|> do ret <- try $ brackets (sepBy (whiteSpace *> matches <* whiteSpace) (symbol "|"))
                  return $ ExpectedMsg ret




hexList ::  CharParser () [Word8]
hexList = brackets (sepBy hexNum (symbol ","))



-- TODO: check if a native Parsec function exits for this
parseString :: GenParser Char st String
parseString = char '"' *> many (noneOf "\"") <* char '"'



hexNum :: GenParser Char st Word8
hexNum = do s <- many1 (oneOf (['0'..'9']++['a'..'f']++['A'..'F']))
            return $ string2hex s

hexNum16 :: GenParser Char st Word16
hexNum16 = do s <- many1 (oneOf (['0'..'9']++['a'..'f']++['A'..'F']))
              return $ string2hex16 s


run :: Show a => Parser a -> String -> IO ()
run p input =
    case parse p "" input of
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



