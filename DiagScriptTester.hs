module DiagScriptTester where

import DiagScriptParser
import DiagnosticConfig
import Control.Monad
import Maybe(fromJust)
import DiagClient
import System(getArgs)
import qualified Test.Framework as TF -- (defaultMainWithOpts, testGroup, Test)
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as HUnit

main = do 
  (f:_) <- getArgs
  diagScript <- readFile f
  case parseScript diagScript of 
    Left err -> do  putStrLn "Error parsing diagScript:"
                    print err
    Right script -> do
      putStrLn $ "testing " ++ f
      let scriptTests = extractTests script
      TF.defaultMainWithArgs scriptTests []

extractTests ::  DiagScript -> [TF.Test]
extractTests (DiagScript s) = map extractTest s

extractTest :: ScriptElement -> TF.Test
extractTest (ScriptTestCase t) = testCase (caseName t) (test2case t)
extractTest (Group n xs) = TF.testGroup n (map extractTest xs)
extractTest (Loop n cnt xs) =
  TF.testGroup ("Loop " ++ n ++ " (" ++ show cnt ++ ")") $
    (join . replicate cnt) (map extractTest xs)

test2case ::  TestCase -> IO ()
test2case (TestCase n msg exp time s t) = do
  resp <- sendDataTo conf (diagPayload msg) s t
  maybe (return ()) (\m->
    HUnit.assertEqual n exp m) resp


runDiagScript :: DiagScript -> IO ()
runDiagScript (DiagScript []) = return ()
runDiagScript (DiagScript es) = do
    mapM_ runElem es
      where runElem (ScriptTestCase t) = runTest t
            runElem (Loop n m xs) = replicateM_ m (mapM runElem xs)
            runElem (Group n xs) = mapM_ runElem xs

runTest :: TestCase -> IO ()
runTest (TestCase n msg exp time s t) =
  sendData conf (diagPayload msg) >> return ()

test ::  FilePath -> IO (Maybe DiagScript)
test f = do
  inpStr <- readFile f
  case parseScript inpStr of 
    Left err -> do  putStrLn "Error parsing input:"
                    print err
                    return Nothing
    Right r -> return $ Just r
