module Diagnoser.DiagScriptTester where

import Control.Monad
import System.Environment (getArgs)
import qualified Test.Framework as TF -- (defaultMainWithOpts, testGroup, Test)
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as HUnit
import Control.Concurrent
import Com.DiagClient
import Diagnoser.ScriptDatatypes
import Diagnoser.Matcher
import DiagnosticConfig
import qualified Diagnoser.DiagScriptParser as P


-- run with: runhaskell DiagScriptTester.hs "Script/nvramtest.skr" "10.40.39.68"

main = do 
  (f:ip:_) <- getArgs
  runTestScript f ip

runTestScript ::  FilePath -> String -> IO ()
runTestScript f ip = do
  diagScript <- readFile f
  case P.parseScript diagScript of 
    Left err -> error $ "Error parsing diagScript:" ++ show err
    Right script -> do
      putStrLn $ "testing " ++ f
      let scriptTests = extractTests script ip
      TF.defaultMainWithArgs scriptTests []

extractTests ::  DiagScript -> String -> [TF.Test]
extractTests (DiagScript s) ip = map (extractTest ip) s

extractTest :: String -> ScriptElement -> TF.Test
extractTest ip (ScriptTestCase t) = testCase (caseName t) ((test2case t ip) >> return ())
extractTest ip (Group n xs) = TF.testGroup n (map (extractTest ip) xs)
extractTest ip (Loop n cnt xs) =
  TF.testGroup ("Loop " ++ n ++ " (" ++ show cnt ++ ")") $
    (join . replicate cnt) (map (extractTest ip) xs)
-- extractTest ip (Wait time) = testCase "wait" (threadDelay (1000 * time))

test2case ::  TestCase -> String -> IO ()
test2case (TestCase n msg exp time Nothing Nothing) ip = (HUnit.assertString "No Source and target specified")   -- use stuff of default config here
test2case (TestCase n msg exp time (Just s) (Just t)) ip = do
  let conf = MkDiagConfig ip 6801 s t False standardDiagTimeout
  resp <- sendDataTo conf (diagPayloadM msg) s t
  if length resp == 0 then (HUnit.assertString "no response received")
  	else (HUnit.assertBool n (matches (head resp) exp))
