module Test.DiagScriptTester where

import Test.DiagScriptParser
import Com.DiagClient(sendData,diagPayload,DiagConfig(MkDiagConfig))
import Control.Monad
import Data.Maybe(fromJust)
import Com.DiagClient
import System.Environment (getArgs)
import qualified Test.Framework as TF -- (defaultMainWithOpts, testGroup, Test)
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as HUnit
import Control.Monad.Reader
import Data.Monoid(mempty)
import DiagnosticConfig

-- run with: runhaskell DiagScriptTester.hs "Script/nvramtest.skr" "10.40.39.68"
main = do 
  (f:ip:_) <- getArgs
  runTestScript f ip

-- TODO: implement glob-patterns for matches

runTestScript ::  FilePath -> String -> IO ()
runTestScript f ip = do
  diagScript <- readFile f
  case parseScript diagScript of 
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

test2case ::  TestCase -> String -> IO ()
test2case (TestCase n msg exp time s t) ip = do
  let conf = MkDiagConfig ip 6801 s t False standardDiagTimeout
  resp <- sendDataTo conf (diagPayload msg) s t
  -- maybe (return ()) (\m->
  maybe (HUnit.assertString "no response received") (\m->
    HUnit.assertEqual n exp m) resp

