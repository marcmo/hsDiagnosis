import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Data.Char
import Util
import Text.ParserCombinators.Parsec.Language
import ParserUtil
import Control.Monad
import Data.Word(Word8)

data ScriptElement = ScriptTestCase TestCase
                   | Loop String Int [ScriptElement]
                   | Group String [ScriptElement]
  deriving Show
data DiagScript = DiagScript {
  scriptElements :: [ScriptElement]
} deriving (Show)
data TestCase = TestCase {
  caseName :: String,
  sendMsg  :: [Word8],
  expected :: [Word8],
  timeout  :: Int,
  source   :: Word8,
  target   :: Word8
} deriving (Show)

runDiagScript :: DiagScript -> IO ()
runDiagScript (DiagScript []) = return ()
runDiagScript (DiagScript es) = do
    mapM_ runElem es
      where runElem (ScriptTestCase t) = runTest t
            runElem (Loop n m xs) = replicateM_ m (mapM runElem xs)
            runElem (Group n xs) = mapM_ runElem xs

runTest :: TestCase -> IO ()
runTest (TestCase n msg exp time s t) =
  print $ n ++ "[" ++ show msg ++ "]"

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
namechars = ['a'..'z']++['A'..'Z']++"_,- "
nameInBrackets = brackets (many1 $ oneOf namechars)
diagscript :: Parser DiagScript
diagscript = do
    whiteSpace
    DiagScript <$> many1 scriptelem <* eof

scriptelem :: Parser ScriptElement
scriptelem = do reserved "LOOPSTART"
                p <- nameInBrackets
                reserved "COUNT"
                n <- brackets (many1 digit)
                cases <- many testcase
                reserved "LOOPEND"
                brackets (string p)
                return $ Loop p (read n) (map ScriptTestCase cases)
         <|> do reserved "GROUPSTART"
                n <- nameInBrackets
                ss <- many1 scriptelem
                reserved "GROUPEND"
                brackets (string n)
                return $ Group n ss
         <|> ScriptTestCase <$> testcase
         <?> "scriptelement"
                
testcase :: Parser TestCase
testcase =
   TestCase <$> (reserved "DIAG" *> nameInBrackets)
            <*> (reserved "SEND" *> hexList)
            <*> (reserved "EXPECT" *> hexList)
            <*> (reserved "TIMEOUT" *> read `fmap` brackets (many1 digit))
            <*> (reserved "SOURCE" *> brackets hexNum)
            <*> (reserved "TARGET" *> brackets hexNum)

hexList ::  CharParser () [Word8]
hexList = brackets $ (sepBy hexNum (symbol ","))

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

main = do 
  contents <- getContents
  runLex diagscript contents

parseScript ::  String -> Either ParseError DiagScript
parseScript = parse diagscript "(unknown)"

test ::  FilePath -> IO (Maybe DiagScript)
test f = do
  inpStr <- readFile f
  case parseScript inpStr of 
    Left err -> do  putStrLn "Error parsing input:"
                    print err
                    return Nothing
    Right r -> return $ Just r
