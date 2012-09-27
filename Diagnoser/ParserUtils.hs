-- | Small module for stuff the DiagScriptParser as well as the PreProcessor depend on.
module Diagnoser.ParserUtils where
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Control.Applicative

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
stringLiteral = P.stringLiteral lexer


eol =  try (string "\r\n")
   <|> string "\n"
   <|> string "\r"
   <?> "End of Line"

nameInBrackets = brackets (many1 $ noneOf "\"\r\n[]")
