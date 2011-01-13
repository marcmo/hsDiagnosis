module Test.DiagScriptTester where

import Test.DiagScriptParser
import DiagnosticConfig
import Control.Monad
import Data.Maybe(fromJust)
import Com.DiagClient
import System.Environment (getArgs)
import qualified Test.Framework as TF -- (defaultMainWithOpts, testGroup, Test)
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as HUnit
import Control.Monad.Reader
import Data.Monoid(mempty)

-- run with: runhaskell DiagScriptTester.hs "Script/nvramtest.skr"
main = do 
  (f:_) <- getArgs
  runTestScript f

-- TODO: implement glob-patterns for matches

runTestScript ::  FilePath -> IO ()
runTestScript f = do
  diagScript <- readFile f
  case parseScript diagScript of 
    Left err -> do putStrLn "Error parsing diagScript:"
                   print err
    Right script -> do
      putStrLn $ "testing " ++ f
      let scriptTests = extractTests script
      TF.defaultMainWithArgs scriptTests []

extractTests ::  DiagScript -> [TF.Test]
extractTests (DiagScript s) = map extractTest s

extractTest :: ScriptElement -> TF.Test
extractTest (ScriptTestCase t) = testCase (caseName t) ((test2case t) >> return ())
extractTest (Group n xs) = TF.testGroup n (map extractTest xs)
extractTest (Loop n cnt xs) =
  TF.testGroup ("Loop " ++ n ++ " (" ++ show cnt ++ ")") $
    (join . replicate cnt) (map extractTest xs)

test2case ::  TestCase -> IO ()
test2case (TestCase n msg exp time s t) = do
  resp <- sendDataTo conf (diagPayload msg) s t
  -- maybe (return ()) (\m->
  maybe (HUnit.assertString "no response received") (\m->
    HUnit.assertEqual n exp m) resp

