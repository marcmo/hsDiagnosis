module Diagnoser.DiagScriptParser

where 

import Data.Word(Word8,Word16)

import Util.Encoding
import Text.ParserCombinators.Parsec.Language
import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Data.Char(toUpper)
import Diagnoser.ScriptDatatypes

import Diagnoser.ParserUtils


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
      


------------------------------------------------------- remove
-- TODO: make filePath match windows/unix file paths
filePath :: CharParser () FilePath
filePath = many1 $ noneOf "\"\r\n "
----------------------------------------------------

hexListNoBrackets ::  CharParser () [Word8]
hexListNoBrackets = sepBy hexNum (symbol ",")


match ::  GenParser Char () Match
match = do s <- try (char '*')
           return Star 
       <|> do s <- try (count 2 (oneOf (['0'..'9']++['a'..'f']++['A'..'F'])))
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
                  return $ ExpectedPayload ret




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



