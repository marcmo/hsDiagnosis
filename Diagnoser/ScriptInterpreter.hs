module Diagnoser.ScriptInterpreter

where

import Diagnoser.DiagnoserScriptParser
import Diagnoser.TestCaseExecuter
import Diagnoser.DiagnosisTestCase
import Control.Monad

data UserAction = UA {
  userMessage :: String
}
data DiagScript = TestCase |
                  Wait Integer |
                  Callscript FilePath |
                  Useraction
                  -- Groupstart
                  -- Groupend
                  -- Loopstart
                  -- Loopend

runDiagScript = do
  let f = "diagnoserscript.skr"
  inpStr <- readFile f
  case parseInput inpStr of 
    Left err -> do  putStrLn "Error parsing input:"
                    print err
    Right r -> do res <- runTestRun r
                  print res

