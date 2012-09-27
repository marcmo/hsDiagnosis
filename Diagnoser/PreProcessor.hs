-- | The PreProcessor calls scripts with parameters as specified by the Diagnoser CALLSCRIPT elements, making them aviable to the parse.
-- It's needed to keep the the actual parser Diagnoser.DiagScriptParser pure.
module Diagnoser.PreProcessor (preProcess)

where

import Diagnoser.ParserUtils
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Control.Applicative
import qualified Data.Map as Map
import Data.List(intercalate)
import Data.Either(lefts,rights)
import qualified System.FilePath as FP-- (isAbsolute,combine)


-- TODO ADD further Script elements for replacment if needed

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
relevants = many relevantOrNot <* eof

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
                 return $ uncurry (Diag name send expect timeout) snt
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
                  canId <- reserved "ID"                *> brackets paraName
                  dat   <- reserved "DATA"              *> paraNameList
                  c     <- reserved "CYCLE"             *> brackets paraName
                  return $ CyclicCanMsg name canId dat c

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
  return (name,var)

nameValuePairList ::  Parser [(String,String)]
nameValuePairList = brackets $ sepBy nameValuePair (symbol ";")

-- end of parser -------------------------------------------------------------------------

replaceParameters :: [(String,String)] -> RelevantOrNot -> RelevantOrNot
replaceParameters _ (Irrelevant s) = Irrelevant s
replaceParameters nameValPairs (Diag name send expect timeout source target) =
  Diag name
       (replaceParameterList nameValPairs send)
       (replaceParameterList nameValPairs expect)
       (replaceParameter     nameValPairs timeout)
       (fmap (replaceParameter nameValPairs) source)
       (fmap (replaceParameter nameValPairs) target)
replaceParameters nameValPairs (CanMsg name canId dat) =
  CanMsg name (replaceParameter nameValPairs canId) (replaceParameterList nameValPairs dat)
replaceParameters nameValPairs (CyclicCanMsg name canId dat cyc) =
  CyclicCanMsg name (replaceParameter nameValPairs canId) (replaceParameterList nameValPairs dat) (replaceParameter nameValPairs cyc)
replaceParameters nameValPairs (CallScript name nameValPairsChild) = CallScript name $ replaceCallScriptParameterList nameValPairs nameValPairsChild



replaceCallScriptParameter :: [(String,String)] -> (String,String) -> (String,String)
replaceCallScriptParameter nvs (n,v) = (n, Map.findWithDefault v v (Map.fromList nvs))

replaceCallScriptParameterList :: [(String,String)] -> [(String,String)] -> [(String,String)]
replaceCallScriptParameterList nvs = map (replaceCallScriptParameter nvs)

replaceParameter :: [(String,String)] -> String -> String
replaceParameter ps n = Map.findWithDefault n n (Map.fromList ps)

replaceParameterList :: [(String,String)] -> [String] -> [String]
replaceParameterList ps = map (replaceParameter ps)



showRelevant :: FilePath -> RelevantOrNot -> IO (Either ParseError String)
showRelevant _ (Irrelevant s) = return $ Right s
showRelevant _ (Diag name send expect timeout source target) = return $ Right $
  "DIAG "     ++ bracketed name                               ++
  " SEND "    ++ bracketed  (intercalate "," send)   ++
  " EXPECT "  ++ bracketed  (intercalate "," expect) ++
  " TIMEOUT " ++ bracketed timeout  ++ sourceAndTarget source  target
    where sourceAndTarget (Just src) (Just tgt)  = " SOURCE " ++ bracketed src ++ " TARGET "  ++ bracketed tgt
          sourceAndTarget _ _ = ""

showRelevant _ (CanMsg name canId dat) = return $ Right $
  "CANMSG " ++ bracketed name     ++
  " ID "    ++ bracketed canId       ++
  " DATA "  ++ bracketedList dat
showRelevant _ (CyclicCanMsg name canId dat cyc) = return $ Right $
  "STARTCYCLICCANMSG " ++ bracketed name ++
  " ID "               ++ bracketed canId ++
  " DATA "             ++ bracketedList dat  ++
  " cyc "            ++ bracketed cyc
showRelevant parentFilePath (CallScript path nameValPairs) =
        prePro newFilePath nameValPairs
  where newFilePath = let dir = FP.dropFileName parentFilePath in
                                FP.combine dir path

bracketed s =  "[" ++ s ++ "]"
bracketedList s = bracketed $ intercalate "," s

prePro :: FilePath -> [(String,String)] -> IO (Either ParseError String)
prePro path nameValPairs =
  do script <- readFile path
     either (return . Left)
            (combineRelevants . map (replaceParameters  nameValPairs))
            (parse relevants "Seperate Irreveant from Relevant lines" script)
  where combineRelevants :: [RelevantOrNot] -> IO (Either ParseError String)
        combineRelevants items = do is <- mapM (showRelevant path) items
                                    if null (lefts is)
                                      then  return . Right $ intercalate "\n"  (rights is)
                                      else  return . Left  $ head              (lefts  is)


-- | Preprocess the file stored in filePath.
preProcess :: FilePath -> IO (Either ParseError String)
preProcess path = prePro path []

