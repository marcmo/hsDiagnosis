
module Diagnoser.DiagScriptTester (runTestScript) where

import Control.Monad
import System.Environment (getArgs)
import qualified Test.Framework as TF -- (defaultMainWithOpts, testGroup, Test)
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as HUnit
import Control.Concurrent
import Com.DiagClient
import Diagnoser.ScriptDatatypes
import Diagnoser.Matcher
--import DiagnosticConfig
import qualified Diagnoser.DiagScriptParser as P


-- run with: runhaskell DiagScriptTester.hs "Script/nvramtest.skr" "10.40.39.68"

-- main = do 
--   (f:ip:_) <- getArgs
--   runTestScript f ip

-- | Run the script stored in file f on the device with ip.
runTestScript ::  FilePath -> DiagConfig -> IO ()
runTestScript f conf = do
  diagScript <- readFile f
  case P.parseScript diagScript of 
    Left err -> error $ "Error parsing diagScript:" ++ show err
    Right script -> do
      putStrLn $ "testing " ++ f
      let scriptTests = extractTests script conf
      TF.defaultMainWithArgs scriptTests []

extractTests ::  DiagScript -> DiagConfig -> [TF.Test]
extractTests (DiagScript s) conf = map (extractTest conf) s



extractTest :: DiagConfig -> ScriptElement -> TF.Test
extractTest conf (ScriptTestCase t) = testCase (caseName t) ((test2case t conf) >> return ())
extractTest conf (Group n xs) = TF.testGroup n (map (extractTest conf) xs)
extractTest conf (Loop n cnt xs) =
  TF.testGroup ("Loop " ++ n ++ " (" ++ show cnt ++ ")") $
    (join . replicate cnt) (map (extractTest conf) xs)
-- extractTest ip (Wait time) = testCase "wait" (threadDelay (1000 * time))

test2case ::  TestCase -> DiagConfig -> IO ()
test2case (TestCase n msg exp time Nothing Nothing) conf@(MkDiagConfig _ _ source target _ _) = 
  test2case (TestCase n msg exp time (Just source) (Just target)) conf
-- (HUnit.assertString "No Source and target specified")   -- use stuff of default config here
test2case (TestCase n msg exp time (Just s) (Just t)) conf = do
--  let conf = MkDiagConfig ip 6801 s t False standardDiagTimeout
  print $ "conf  "++ show conf 
  resp <- sendDataTo conf (diagPayloadM msg) s t
  if length resp == 0 then (HUnit.assertString "no response received")
  	else (HUnit.assertBool n (matches (head resp) exp))
