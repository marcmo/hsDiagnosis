module Tests.DiagnoserScriptParserTests
  (
      diagnoserScripterTests
     ,devTest
  )
where

import qualified Diagnoser.DiagScriptParser as SP
import Diagnoser.ScriptInterpreter
import Com.DiagMessage
import qualified Test.HUnit as HU
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Text.Parsec.Error
import Diagnoser.DiagScriptParser
import Util.RecursiveContents
import Control.Monad

import qualified Text.Show.Pretty as Pr

scriptPath = "Tests/diagnoser/implemented"

diagnoserScripterTests :: IO Test
diagnoserScripterTests = do
  testCaseAssertion         <- assertionTest testCaseExplicitResult      (scriptPath ++ "/diagExplicit.skr")
  testCaseNoSTAssertion     <- assertionTest testCaseNoSTResult          (scriptPath ++ "/diagNoSourceTarget.skr")
  testCaseNoneAssertion     <- assertionTest testCaseNoneResult          (scriptPath ++ "/diagNone.skr") -- working but shouldn't
  testCaseEveryAssertion    <- assertionTest testCaseEveryResult         (scriptPath ++ "/diagEvery.skr")
  testCaseEoNAssertion      <- assertionTest testCaseEveryOrNoneResult   (scriptPath ++ "/diagEveryOrNone.skr")
  testCaseStarAssertion     <- assertionTest testCaseStarResult          (scriptPath ++ "/diagStar.skr")
  testCaseQuestionAssertion <- assertionTest testCaseQuestionmarkResult  (scriptPath ++ "/diagQuestionmark.skr")
  testCaseQHalfAssertion    <- assertionTest testCaseQuestionHalfResult  (scriptPath ++ "/diagQuestionHalf.skr")
  testCaseWildcardAssertion <- assertionTest testCaseWildcardResult      (scriptPath ++ "/diagWildcard.skr")
  testCaseOneHexAssertion   <- assertionTest testOneHexResult            (scriptPath ++ "/diagOneHex.skr")
  testCaseOrAssertion       <- assertionTest testOrResult                (scriptPath ++ "/diagOr.skr")



  loopAssertion             <- generalTest   testLoopExplicit        (scriptPath ++ "/loopSimple.skr")
  loopNestedAssertion       <- assertionTest loopNestedResult        (scriptPath ++ "/loopNested.skr")
  groupNestedAssertion      <- assertionTest groupNestedResult       (scriptPath ++ "/groupNested.skr")
  groupNumberNameAssertion  <- assertionTest groupNumberNameResult   (scriptPath ++ "/groupNumberName.skr")
  waitSimpleAssertion       <- assertionTest waitSimpleResult        (scriptPath ++ "/waitSimple.skr")
  useractionSimpleAssertion <- assertionTest useractionSimpleResult  (scriptPath ++ "/useractionSimple.skr")
  callscriptSimpleAssertion <- assertionTest callscriptSimpleResult  (scriptPath ++ "/callscriptSimple.skr")
  callscriptWithParameterAssertion <- assertionTest callscriptWithParameterResult   (scriptPath ++ "/callscriptWithParameter.skr")
  callscriptWithParametersAssertion <- assertionTest callscriptWithParametersResult (scriptPath ++ "/callscriptWithParameters.skr")
  canmsgSimpleAssertion     <- assertionTest canmsgSimpleResult      (scriptPath ++ "/canmsgSimple.skr")
  canmsgCyclicAssertion     <- assertionTest canmsgCyclicResult      (scriptPath ++ "/canmsgCyclic.skr")
  
  waitExample       <- assertionTest allTrueResult              "Tests/diagnoser/Beispiele_WAIT/EXAMPLE_kwp2000_test_with_WAIT.skr"


  return $ testGroup "diagnoser-script Group" [
                        testGroup "testCase (DIAG)"
                                  [testCase "testCase construct (test written expcicitly)"        testCaseAssertion
                                  ,testCase "testCase construct (WITHOUT source and target)"      testCaseNoSTAssertion
                                  ,testCase "testCase construct (expected none)"                  testCaseNoneAssertion
                                  ,testCase "testCase construct (expected every)"                 testCaseEveryAssertion
                                  ,testCase "testCase construct (expected every or none)"         testCaseEoNAssertion
                                  ,testCase "testCase construct (with star wildcard)"             testCaseStarAssertion
                                  ,testCase "testCase construct (with questionmarks wildcard)"    testCaseQuestionAssertion
                                  ,testCase "testCase construct (with one questionmark wildcard)" testCaseQHalfAssertion
                                  ,testCase "testCase construct (with all wildcards)"             testCaseWildcardAssertion
                                  ,testCase "testCase construct (with only one hex number in skr)"testCaseOneHexAssertion
                                  ,testCase "testCase construct (with or in expected)"            testCaseOrAssertion
                                  ],
                        testGroup "loops"
                                  [testCase "simple loop construct (test written expcicitly)" loopAssertion,
                                   testCase "nested loop construct" loopNestedAssertion],
                        testGroup "groups"
                                  [testCase "nested group construct" groupNestedAssertion,
                                   testCase "simple group construct with a number as name" groupNumberNameAssertion],
                        testGroup "wait"
                                  [testCase "simple wait construct" waitSimpleAssertion],
                        testGroup "useraction"
                                  [testCase "simple useraction construct" waitSimpleAssertion],
                        testGroup "callscript"
                                  [testCase "simple callscript construct" callscriptSimpleAssertion,
                                   testCase "simple callscript construct with one Parameters" callscriptWithParameterAssertion,
                                   testCase "simple callscript construct with Parameters" callscriptWithParametersAssertion],
                        testGroup "canmsg"
                                  [testCase "simple canmsg construct"   canmsgSimpleAssertion,
                                   testCase "cyclic canmsg construct"   canmsgCyclicAssertion],
                        testGroup "Examples"
                                  [testCase "Wait Example" waitExample]
         ]

type ParsedSkript    = Either ParseError DiagScript
type SkriptAssertion = ParsedSkript -> HU.Assertion 


testOrResult :: DiagScript -> Bool
testOrResult (DiagScript 
                       [ScriptTestCase ( 
                         TestCase _ 
                            (DiagScriptMsg Nothing Nothing _)
                            (ExpectedMessage Nothing Nothing (ExpectedMsg [_,_])) 
                            _ Nothing Nothing )])                           = True
testOrResult _                                                              = False


testCaseNoSTResult :: DiagScript -> Bool
testCaseNoSTResult (DiagScript 
                       [ScriptTestCase ( 
                         TestCase "11_ECU_RESET_POWERON"
                           (DiagScriptMsg Nothing Nothing [0x11,0x01])
                           (ExpectedMessage Nothing Nothing _) 
                           2000 Nothing Nothing )])                           = True
testCaseNoSTResult _                                                   = False


testCaseNoneResult :: DiagScript -> Bool
testCaseNoneResult (DiagScript 
                       [ScriptTestCase ( 
                         TestCase "abc" _
                           (ExpectedMessage _ _ NoMsg) 
                           2000 _ _ )])                           = True
testCaseNoneResult _                                                   = False

testCaseEveryResult :: DiagScript -> Bool
testCaseEveryResult (DiagScript 
                      [ScriptTestCase ( 
                         TestCase "abc" 
                           (DiagScriptMsg _ _ [0x1,0x2,0x3]) 
                           (ExpectedMessage _ _ EveryMsg)  
                           2000 _ _)])                          = True
testCaseEveryResult _                                           = False

testCaseEveryOrNoneResult :: DiagScript -> Bool
testCaseEveryOrNoneResult (DiagScript 
                             [ScriptTestCase ( 
                                TestCase "abc"                            
                                  (DiagScriptMsg _ _ _) 
                                  (ExpectedMessage _ _ EveryOrNoMsg)  
                                  2000 _ _)])                     = True
testCaseEveryOrNoneResult _                                       = False

testCaseQuestionmarkResult :: DiagScript -> Bool
testCaseQuestionmarkResult (DiagScript 
                             [ScriptTestCase ( 
                               TestCase _ _ 
                                (ExpectedMessage (Just 0xB0) (Just 0xA0) 
                                                 (ExpectedMsg [[Match 0xaa,Questioned "??",Match 0xcc]]))  
                               _ _ _)])                                            = True
testCaseQuestionmarkResult _                                                       = False

testCaseQuestionHalfResult :: DiagScript -> Bool
testCaseQuestionHalfResult (DiagScript 
                             [ScriptTestCase ( 
                               TestCase _ _ 
                                 (ExpectedMessage _ _ (ExpectedMsg [[Match 0xaa,Questioned "F?",Match 0xcc]]))
                                  _ _ _)])                                         = True
testCaseQuestionHalfResult _                                                       = False

testCaseStarResult :: DiagScript -> Bool
testCaseStarResult (DiagScript 
                        [ScriptTestCase ( 
                          TestCase "abc" (DiagScriptMsg _ _ [0x1,0x2,0x3])
                            (ExpectedMessage _ _ (ExpectedMsg [[Match 0xaa, Star ,Match 0xcc]]))
                             2000 _ _)])                                           = True
testCaseStarResult _                                                               = False



testCaseWildcardResult :: DiagScript -> Bool
testCaseWildcardResult (DiagScript 
                        [ScriptTestCase ( 
                          TestCase "abc" _ _ 2000 _ _)])                        = True
testCaseWildcardResult _                                                           = False



testCaseExplicitResult :: DiagScript -> Bool
testCaseExplicitResult (DiagScript 
                        [ScriptTestCase ( 
                         TestCase "abc"
                          (DiagScriptMsg (Just 0xA0) (Just 0xB0) [0x1,0x2,0x3])
                          (ExpectedMessage (Just 0xB0) (Just 0xA0) (ExpectedMsg [[Match 0xaa,Match 0xbb,Match 0xcc]]))
                          2000 (Just 0xA0) (Just 0xB0))]) = True
testCaseExplicitResult _         = False

testOneHexResult :: DiagScript -> Bool
testOneHexResult (DiagScript 
                   [ScriptTestCase ( 
                     TestCase _ (DiagScriptMsg (Just 0xf0) (Just 0xf2) [0x31,0x0])
                                   (ExpectedMessage  (Just 0xf2) (Just 0xf0) (ExpectedMsg [[Match 1,Match 2, Match 3]]))
                                 _ _ _)])                                     = True
testOneHexResult  _                                                           = False


canmsgCyclicResult :: DiagScript -> Bool
canmsgCyclicResult (DiagScript [CyclicCanMsg "Klemme_15" 0x130 [0x05,0x00,0x00,0x00] 200 [Wait 1000]]) = True
canmsgCyclicResult _                   = False


canmsgSimpleResult :: DiagScript -> Bool
canmsgSimpleResult (DiagScript [CanMsg "CAN_1" 0x6F1 [0x11,0x22,0x33,0x44]]) = True
canmsgSimpleResult _                                                            = False


callscriptSimpleResult :: DiagScript -> Bool
callscriptSimpleResult (DiagScript [Callscript "EXAMPLE_Script_CALLSCRIPT_Target.skr" []]) = True


callscriptWithParameterResult :: DiagScript -> Bool
callscriptWithParameterResult (DiagScript [Callscript _ [_]]) = True
callscriptWithParameterResult _                   = False


callscriptWithParametersResult :: DiagScript -> Bool
callscriptWithParametersResult (DiagScript [Callscript _ [_,_,_]]) = True
callscriptWithParametersResult _                   = False



useractionSimpleResult :: DiagScript -> Bool
useractionSimpleResult (DiagScript [Useraction "Dieser Text wird als MsgBox angezeigt!"]) = True
useractionSimpleResult _                   = False

waitSimpleResult :: DiagScript -> Bool
waitSimpleResult (DiagScript [Wait 1000]) = True
waitSimpleResult  _                = False 


loopNestedResult :: DiagScript -> Bool
loopNestedResult (DiagScript 
                   [Loop "loopA" 10 
                     [ScriptTestCase _,
                       Loop "loopB" 10 
                         [ScriptTestCase _]]]) = True
loopNestedResult  _                               = False 


groupNumberNameResult :: DiagScript -> Bool
groupNumberNameResult (DiagScript 
                          [Group "1" _ ]) = True
groupNumberNameResult  _                     = False 


groupNestedResult :: DiagScript -> Bool
groupNestedResult (DiagScript 
                        [Group "g" 
                          [ScriptTestCase _, 
                            Group "h" 
                             [ScriptTestCase _]]]) = True
groupNestedResult  _                                  = False 

         
testLoopExplicit ::  SkriptAssertion
testLoopExplicit parseResult =
  let diagMsg       = DiagScriptMsg (Just 0xA0) (Just 0xB0) [0x1,0x2,0x3]
      diagMsgExpect = ExpectedMessage  (Just 0xB0) (Just 0xA0) (ExpectedMsg [[Match 0xaa, Match 0xbb, Match 0xcc]])
      expected      = DiagScript {
        scriptElements = [
          Loop "loopA" 10 [
            ScriptTestCase (
              TestCase "abc" diagMsg diagMsgExpect 2000 (Just 0xA0) (Just 0xB0)) ]]}
      mkTest = either
          (\error -> HU.assertBool ("was not parsed correctly:" ++ show error) False)
          (\x -> expected HU.@=? x) in
  mkTest parseResult


generalAssertion :: ParsedSkript -> (DiagScript -> Bool) -> HU.Assertion 
generalAssertion parseResult checkResult =
  let mkTest = either
         (\error -> HU.assertBool ("was not parsed correctly:" ++ show error) False)                       
         (HU.assertBool "parsed correctly:" . checkResult)   in
  mkTest parseResult 


assertionTest testAssertion file  = do
  f <-  readFile file
  let s = SP.parseScript f 
  return $ generalAssertion s testAssertion                  


generalTest :: SkriptAssertion -> FilePath -> IO HU.Assertion
generalTest testAssertion file  = do
  f <-  readFile file
  let s = SP.parseScript f 
  return $ testAssertion s

allTrueResult :: DiagScript -> Bool
allTrueResult _ = True

allSkriptFiles = getRecursiveContents "Tests/diagnoser/"

 

-- Function for quickly comparing input from .skr file and parsed Output
devTest :: String -> IO ()
devTest f = do
  s <- readFile f
  let p = SP.parseScript s  
  putStrLn s 
  print p 






devtests = do a <- allSkriptFiles
              (mapM_ devTest a)

devTests2 = do a <- allSkriptFiles
               (mapM_ dTest a)
      where dTest f = do  s <- readFile f
                          let p = SP.parseScript s  
                          putStrLn "\n\n\n"
                          putStrLn "---------------------------------------------------------------------------------"
                          putStrLn f
                          putStrLn s 
                          print p 



-- error in: EXAMPLE_nested_groups_cycliccanmsg.skr