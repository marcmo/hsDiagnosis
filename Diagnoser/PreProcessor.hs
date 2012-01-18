module Diagnoser.PreProcessor where

import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Control.Applicative

import System.Directory
import qualified System.FilePath as FP-- (isAbsolute,combine)

import qualified Data.Map as Map
import Data.List
import Data.Either

import Diagnoser.ParserUtils
import Debug.Trace

-- TODO ADD further Script elements for replacment: CyclicCanMsg etc.

data RelevantOrNot = Irrelevant String             -- any other constructor is relevant for replacing parameters
                   | CallScript FilePath 
                                [(String,String)]  -- name value pairs
                   | Diag  String                  -- name 
                           [String]                -- send
                           [String]                -- expect 
                           String                  -- timeout
                           (Maybe String)          -- source
                           (Maybe String)          -- target
                   | CanMsg String String [String] 
                   | CyclicCanMsg String String [String] String
                   deriving (Show)


relevants :: Parser [RelevantOrNot]
relevants = do (many relevantOrNot) <* eof

relevantOrNot :: Parser RelevantOrNot
relevantOrNot =  try (relevant <* eol)
             <|> try  relevant                               -- for the last line
             <|> Irrelevant <$> try (manyTill anyChar eol)
             <|> Irrelevant <$>      many1    anyChar        -- for the last line
             <?> "Relevant Or Irrelevant Line" 

relevant :: Parser RelevantOrNot
relevant =  try callScript
        <|> try canMsg
        <|> try cyclicCanMsg
        <|> diag

diag :: Parser RelevantOrNot
diag        = do name    <- reserved "DIAG"    *> nameInBrackets
                 send    <- reserved "SEND"    *> paraNameList
                 expect  <- reserved "EXPECT"  *> paraNameList
                 timeout <- reserved "TIMEOUT" *> brackets paraName
                 snt     <- sourceAndTarget
                 return $ Diag name send expect timeout (fst snt) (snd snt)
    where sourceAndTarget :: CharParser () (Maybe String, Maybe String)
          sourceAndTarget = do source <- reserved "SOURCE" *> brackets paraName
                               target <- reserved "TARGET" *> brackets paraName
                               return (Just source, Just target)
                         <|> return (Nothing, Nothing)                


canMsg :: Parser RelevantOrNot
canMsg = CanMsg <$> (reserved "CANMSG"  *> nameInBrackets)         
                <*> (reserved "ID"      *> brackets paraName)         
                <*> (reserved "DATA"    *> paraNameList)

cyclicCanMsg :: Parser RelevantOrNot
cyclicCanMsg = do name  <- reserved "STARTCYCLICCANMSG" *> nameInBrackets
                  id    <- reserved "ID"                *> brackets paraName
                  dat   <- reserved "DATA"              *> paraNameList
                  cycle <- reserved "CYCLE"             *> brackets paraName
--                  ss    <- many scriptelem              <* 
--                           reserved "STOPCYCLICCANMSG"  <* brackets (string name)  -------- just an irrelevant line
                  return $ CyclicCanMsg name id dat cycle

paraNameList ::  CharParser () [String]
paraNameList = brackets $ sepBy paraName (symbol ",")

paraName ::  CharParser () String
paraName = many1 $ oneOf varNameChars

varNameChars = ['a'..'z']++['A'..'Z']++"_- "++['0'..'9']++"*?"

callScript :: Parser RelevantOrNot
callScript = CallScript <$> (reserved "CALLSCRIPT" *> filePath)
                        <*> (whiteSpace            *> option [] nameValuePairList)


-- TODO: make filePath match windows/unix file paths
filePath :: CharParser () FilePath
filePath = many1 $ noneOf "\"\r\n "

nameValuePair ::  Parser (String,String)
nameValuePair  = do 
  char '"'
  name <- many1 $ oneOf varNameChars
  char '"'; char '=';  char '"'
  var <- many1 $ oneOf  (varNameChars ++ ",")
  char '"'
  return $ (name,var)

nameValuePairList ::  Parser [(String,String)]
nameValuePairList = brackets $ sepBy nameValuePair (symbol ";")

-- end of parser -------------------------------------------------------------------------

replaceParameters :: [(String,String)] -> RelevantOrNot -> RelevantOrNot
replaceParameters nameValPairs (Irrelevant s) = Irrelevant s
replaceParameters nameValPairs (Diag name send expect timeout source target) = 
  Diag name
       (replaceParameterList nameValPairs send)
       (replaceParameterList nameValPairs expect)
       (replaceParameter     nameValPairs timeout)
       (fmap (replaceParameter nameValPairs) source)
       (fmap (replaceParameter nameValPairs) target)
replaceParameters nameValPairs (CanMsg name id dat) = 
  CanMsg name (replaceParameter nameValPairs id) (replaceParameterList nameValPairs dat)
replaceParameters nameValPairs (CyclicCanMsg name id dat cycle) = 
  CyclicCanMsg name (replaceParameter nameValPairs id) (replaceParameterList nameValPairs dat) (replaceParameter nameValPairs cycle)
replaceParameters nameValPairs (CallScript name nameValPairsChild) = CallScript name $ replaceCallScriptParameterList nameValPairs nameValPairsChild



replaceCallScriptParameter :: [(String,String)] -> (String,String) -> (String,String)
replaceCallScriptParameter nvs (n,v) = (n, Map.findWithDefault v v (Map.fromList nvs))

replaceCallScriptParameterList :: [(String,String)] -> [(String,String)] -> [(String,String)]
replaceCallScriptParameterList nvs ns = map (replaceCallScriptParameter nvs) ns

replaceParameter :: [(String,String)] -> String -> String
replaceParameter ps n = Map.findWithDefault n n (Map.fromList ps)

replaceParameterList :: [(String,String)] -> [String] -> [String]
replaceParameterList ps ns = map (replaceParameter ps) ns



showRelevant :: FilePath -> RelevantOrNot -> IO (Either ParseError String)
showRelevant _ (Irrelevant s) = trace "show irrel: " $ return $ Right s
showRelevant _ (Diag name send expect timeout source target) = return $ Right $
  "DIAG "     ++ (bracketed name)               ++ 
  " SEND "    ++ (bracketed $ concat $ intersperse "," send)   ++ 
  " EXPECT "  ++ (bracketed $ concat $ intersperse "," expect)   ++ 
  " TIMEOUT " ++ (bracketed timeout)  ++ sourceAndTarget source  target
    where sourceAndTarget Nothing Nothing = ""
          sourceAndTarget (Just source)  (Just target)  = " SOURCE "  ++ bracketed source   ++ 
                                                          " TARGET "  ++ bracketed target
showRelevant _ (CanMsg name id dat) = return $ Right $
  "CANMSG " ++ (bracketed name)     ++
  " ID "    ++ (bracketed id)       ++
  " DATA "  ++ (bracketedList dat) 
showRelevant _ (CyclicCanMsg name id dat cycle) = return $ Right $
  "STARTCYCLICCANMSG " ++ (bracketed name) ++
  " ID "               ++ (bracketed id) ++ 
  " DATA "             ++ (bracketedList dat)  ++
  " CYCLE "            ++ (bracketed cycle) 
showRelevant parentFilePath (CallScript filePath nameValPairs) = do script <- readFile newFilePath
                                                                    prePro newFilePath nameValPairs
  where newFilePath = let dir = FP.dropFileName parentFilePath in
                                FP.combine dir filePath

bracketed s =  "[" ++ s ++ "]"
bracketedList s = bracketed $ concat $ intersperse "," s

prePro :: FilePath -> [(String,String)] -> IO (Either ParseError String)
prePro filePath nameValPairs = 
  do script <- readFile (trace ("prePro" ++ show filePath ++ show nameValPairs) filePath)
     let rels = parse relevants "Seperate Irreveant from Relevant lines" script
     case rels of 
       (Left  a) -> return $ Left a
       (Right a) -> (combineRelevants $ map (replaceParameters  nameValPairs) a) 
  where combineRelevants :: [RelevantOrNot] -> IO (Either ParseError String)
        combineRelevants items = do is <- sequence $  map (showRelevant filePath) items 
                                    if null (lefts is) 
                                    then  (return $ Right $ concat $ intersperse "\n" (rights is))
                                    else  (return $ Left  $ head                      (lefts  is))



preProcess :: FilePath -> IO (Either ParseError String)
preProcess filePath = 
    prePro filePath []

