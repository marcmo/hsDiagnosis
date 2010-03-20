module Test.DiagScriptTester where

import Test.DiagScriptParser
import DiagnosticConfig
import Control.Monad
import Maybe(fromJust)
import Com.DiagClient
import System (getArgs)
import qualified Test.Framework as TF -- (defaultMainWithOpts, testGroup, Test)
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as HUnit
import Control.Monad.Reader
-- import Criterion.Main
-- import Criterion.Config
import Data.Monoid(mempty)


-- fibIo :: Int -> IO ()
-- fibIo n = print $ fib n
-- benchmarkConfig :: Config
-- benchmarkConfig = Config {
--                   cfgBanner       = ljust "I don't know what version I am."
--                 , cfgConfInterval = ljust 0.95
--                 , cfgPerformGC    = ljust False
--                 , cfgPlot         = mempty
--                 , cfgPlotSameAxis = ljust False
--                 , cfgPrintExit    = Nada
--                 , cfgResamples    = ljust (100 * 1000)
--                 , cfgSamples      = ljust 100
--                 , cfgSummaryFile  = mempty
--                 , cfgVerbosity    = ljust Normal
--                 }

-- main = defaultMain [
--        bgroup "fib" [ bench "fib 10" $ fibIo 10
--                     , bench "fib 35" $ fibIo 35
--                     ]
--                    ]
-- run with: runhaskell DiagScriptTester.hs "Script/nvramtest.skr"
-- main = do 
--   (f:_) <- getArgs
--   runTestScript f

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

