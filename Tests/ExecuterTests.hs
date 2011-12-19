module Tests.ExecuterTests where

import qualified Test.HUnit as HU
import Diagnoser.ScriptDatatypes
import Diagnoser.Matcher

import qualified Diagnoser.DiagScriptParser as SP
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Data.Either.Unwrap
import Control.Monad
import Util.RecursiveContents
import Diagnoser.TestCaseExecuter

-- TODO: write some additional tests to make sure matcher works correctly
executerTests :: Test
executerTests = 
  testGroup "Matcher"
    [testCase "Match questioned first"  $ assert    $  matchQuestioned  "03" "?3"
    ,testCase "Match questioned second" $ assert    $  matchQuestioned  "33" "?3"
    ,testCase "Matcher1"                $ assert    $ matcher1 [0,1,2] [Match 0, Match 1, Match 2]
    ,testCase "Matcher1 fail"           $ assertNot $ matcher1 [0,1,3] [Match 0, Match 1, Match 2] 
    ,testCase "Questioned"              $ assert    $ matcher1 [0]     [Questioned "0?"] 
    ,testCase "Questioned failed"       $ assertNot $ matcher1 [0]     [Questioned "0?", Match 01] 
    ,testCase "Questioned first"        $ assert    $ matcher1 [0xFF]  [Questioned "?F"] 
    ,testCase "Questioned second"       $ assert    $ matcher1 [0xFF]  [Questioned "F?"] 
    ,testCase "Questioned smallletter s"$ assert    $ matcher1 [0xFF]  [Questioned "f?"] 
    ,testCase "Questioned smallletter f"$ assert    $ matcher1 [0xFF]  [Questioned "?f"]
    ,testCase "matcher simple fail"     $ assertNot $ matcher [0,1,2] (ExpectedPayload [[Match 0, Match 1, Match 3]]) 
    ,testCase "matcher simple star"     $ assert    $ matcher [0,1,2] (ExpectedPayload [[Star   , Match 1, Match 2]])
    ,testCase "matcher simple star fail"$ assertNot $ matcher [0,1,2] (ExpectedPayload [[Star   , Match 2, Match 2]])
    ,testCase "matcher star Fail   "    $ assertNot $ matcher [0,1,2] (ExpectedPayload [[Star   , Match 1, Match 3 ]])
    ,testCase "matcher star Fail2  "    $ assertNot $ matcher [0,1,2] (ExpectedPayload [[Star   , Questioned "?0", Match 3 ]])
    ,testCase "matche star many"        $ assert    $ matcher [0,1,2] (ExpectedPayload [[Star   , Match 2]])
    ,testCase "matcher star end"        $ assert    $ matcher [0xAA,0xBB,0xCC]
                                                               (ExpectedPayload [[Match 0xaa, Star]])
    ,testCase "matcher or Case"         $ assert    $ matcher [0,1,2] (ExpectedPayload [[Star   , Match 2, Match 2],
                                                                                        [Star   , Questioned "?1", Match 2]])
    ,testCase "matcher or Case Fail"    $ assertNot $ matcher [0,1,2] (ExpectedPayload [[Star   , Match 2, Match 2],
                                                                                        [Match 0, Match 2, Match 3 ]])
    ,testCase "matcher many in one"     $ assert    $ matcher [0xAA,0xbb,0xcc,0x01,0x33] 
                                                              (ExpectedPayload [[Star   , Match 2],
                                                                                [Questioned "?A"
                                                                                ,Questioned "B?" 
                                                                                ,Star]])

    ,testCase "Every Message"           $ assert    $ matcher [0,1,2] EveryMsg                                                
    ,testCase "Every Or No Message"     $ assert    $ matcher [0,1,2] EveryOrNoMsg
    ]


assert :: Bool -> HU.Assertion
assert a    = HU.assertBool    "failed, but should not" a

assertNot :: Bool -> HU.Assertion
assertNot a = HU.assertBool "failed not, but should" $ not a






-- Run some simple Scripts with TestCaseExecuter --------------------------------------------------

simpleExamples :: [ScriptElement]
simpleExamples  = 
  [Wait 100
  ,Useraction "This is a Messagle"
  ,Callscript "/testfile.skr" [Parameter "name" [0x03], Parameter "name2" [0x03,0x04,0x05]]
  ,CanMsg     "canmsg1" 0x000 [0x1,0x2,0x3]           
  ,ScriptTestCase (TestCase "TestName" 
                            (DiagScriptMsg (Just 1) (Just 2) [0x00,0x11])         
                            (ExpectedMsg   (Just 2) (Just 1) (ExpectedPayload [[Match 0x00,Match 0x11]]))
                            1000  (Just 1)  (Just 2))
  ,Group "Group_name"  [Useraction "This is a Message in a group"]
  ,Loop  "Loop_name" 3 [Useraction "This is a Message in a Loop"] 
  ]



runScriptEx ex = do 
   runScript (DiagScript [ex])
   putStrLn ""


runScriptExs = mapM_  runScriptEx simpleExamples




secondary = let sent = [0x22,0x20,0x00]
                expected = fmap Match [0x62,0x20,0x0,0x40,0xf,0xff,0xf,0x80,0x40,0x2,0xf,0x80,0x40,0x27,0xf,0x80,0x40,0x2b,0x4c,0x80,0x41,0x88,0xe,0x80,0x41,0xa6,0xf,0x93,0x7,0x1f,0xf] in
            DiagScript [ScriptTestCase (TestCase "Secondary Error Memory" 
                            (DiagScriptMsg Nothing Nothing sent)         
                            (ExpectedMsg   Nothing Nothing (ExpectedPayload [expected]))
                            1000  Nothing  Nothing)]

crap = DiagScript [ScriptTestCase (TestCase "Crap" 
                            (DiagScriptMsg Nothing Nothing [0x00,0x11])         
                            (ExpectedMsg   Nothing Nothing (ExpectedPayload [[Match 0x00,Match 0x11]]))
                            1000  Nothing  Nothing)]

-- runScript secondary



-- Test All Examples with TestCaseExecuter --------------------------------------------------

allSkriptFiles = getRecursiveContents "Tests/diagnoser/"

printScript f = do
  putStrLn "----------------------------------------------------------------"
  putStrLn $"File: " ++  (fst f) ++ "\n"
  runScript (snd f)
  putStrLn "----------------------------------------------------------------"
  return True

example f = do
  s <- readFile f
  let p = SP.parseScript s  
  return p

-- Ugly function that exctracts all correctly parsed skripts, with it's file
examples :: IO [(FilePath,DiagScript)]
examples = do scripts <- allSkriptFiles >>= mapM example
              files   <- allSkriptFiles
              let scriptsNfiles =  zip files scripts
                  rights =  filter (isRight . snd)  scriptsNfiles
              return (zip (fmap fst scriptsNfiles) (fmap (fromRight . snd) rights))

runScripts = examples >>= mapM printScript



