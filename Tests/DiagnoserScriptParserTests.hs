module Tests.DiagnoserScriptParserTests
  (
      diagnoserScripterTests
  )
where

import qualified Diagnoser.DiagScriptParser as SP
import Com.DiagMessage
import qualified Test.HUnit as HU
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Text.Parsec.Error
import Diagnoser.DiagScriptParser

scriptPath = "Tests/diagnoser/implemented"

diagnoserScripterTests :: IO Test
diagnoserScripterTests = do
  testCaseAssertion         <- assertionTest testCaseExplicitResult  (scriptPath ++ "/diagExplicit.skr")
  loopAssertion             <- generalTest   testLoopExplicit        (scriptPath ++ "/loopSimple.skr")
  loopNestedAssertion       <- assertionTest loopNestedResult        (scriptPath ++ "/loopNested.skr")
  groupNestedAssertion      <- assertionTest groupNestedResult       (scriptPath ++ "/groupNested.skr")
  groupNumberNameAssertion  <- assertionTest groupNumberNameResult   (scriptPath ++ "/groupNumberName.skr")
  waitSimpleAssertion       <- assertionTest waitSimpleResult        (scriptPath ++ "/waitSimple.skr")
  useractionSimpleAssertion <- assertionTest useractionSimpleResult  (scriptPath ++ "/useractionSimple.skr")
  callscriptSimpleAssertion <- assertionTest callscriptSimpleResult  (scriptPath ++ "/callscriptSimple.skr")
  callscriptWithParameterAssertion <- assertionTest callscriptWithParameterResult  (scriptPath ++ "/callscriptWithParameter.skr")
  callscriptWithParametersAssertion <- assertionTest callscriptWithParametersResult  (scriptPath ++ "/callscriptWithParameters.skr")
  canmsgSimpleAssertion     <- assertionTest canmsgSimpleResult  (scriptPath ++ "/canmsgSimple.skr")
  canmsgCyclicAssertion     <- assertionTest canmsgCyclicResult  (scriptPath ++ "/canmsgCyclic.skr")

  return $ testGroup "diagnoser-script Group" [
                        testGroup "testCase (DIAG)"
                                  [testCase "testCase (DIAG) construct (test written expcicitly)"  testCaseAssertion],
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
                                   testCase "cyclic canmsg construct"   canmsgCyclicAssertion]

         ]


type ParsedSkript    = Either ParseError SP.DiagScript
type SkriptAssertion = ParsedSkript -> HU.Assertion 
--data SkriptTest      = SkriptTest SkriptAssertion FilePath



testCaseExplicitResult :: DiagScript -> Bool
testCaseExplicitResult (SP.DiagScript 
                        [(SP.ScriptTestCase ( 
                         SP.TestCase "abc" diagMsg diagMsgExpect 2000 0xA0 0xB0))]) = True
                        where 
                          diagMsg       = DiagnosisMessage 0xA0 0xB0 [0x1,0x2,0x3]
                          diagMsgExpect = DiagnosisMessage 0xB0 0xA0 [0xaa,0xbb,0xcc]
testCaseExplicitResult _         = False


tempResult :: DiagScript -> Bool
tempResult (SP.DiagScript [_]) = True
tempResult _                   = False


canmsgCyclicResult :: DiagScript -> Bool
canmsgCyclicResult (SP.DiagScript [CyclicCanMsg "Klemme_15" 0x130 [0x05,0x00,0x00,0x00] 200 [Wait 1000]]) = True
canmsgCyclicResult _                   = False


canmsgSimpleResult :: DiagScript -> Bool
canmsgSimpleResult (SP.DiagScript [CanMsg "CAN_1" 0x6F1 [0x11,0x22,0x33,0x44]]) = True
canmsgSimpleResult _                   = False


callscriptSimpleResult :: DiagScript -> Bool
callscriptSimpleResult (SP.DiagScript [Callscript "EXAMPLE_Script_CALLSCRIPT_Target.skr" []]) = True
callscriptResult _                         = False


callscriptWithParameterResult :: DiagScript -> Bool
callscriptWithParameterResult (SP.DiagScript [Callscript _ [_]]) = True
callscriptWithParameterResult _                   = False


callscriptWithParametersResult :: DiagScript -> Bool
callscriptWithParametersResult (SP.DiagScript [Callscript _ [_,_,_]]) = True
callscriptWithParametersResult _                   = False



useractionSimpleResult :: DiagScript -> Bool
useractionSimpleResult (SP.DiagScript [Useraction "Dieser Text wird als MsgBox angezeigt!"]) = True
useractionSimpleResult _                   = False

waitSimpleResult :: DiagScript -> Bool
waitSimpleResult (SP.DiagScript [Wait 1000]) = True
waitSimpleResult  _                = False 


loopNestedResult :: DiagScript -> Bool
loopNestedResult (SP.DiagScript 
                   [SP.Loop "loopA" 10 
                     [SP.ScriptTestCase _,
                       SP.Loop "loopB" 10 
                         [SP.ScriptTestCase _]]]) = True
loopNestedResult  _                               = False 


groupNumberNameResult :: DiagScript -> Bool
groupNumberNameResult (SP.DiagScript 
                          [SP.Group "1" _ ]) = True
groupNumberNameResult  _                     = False 


groupNestedResult :: DiagScript -> Bool
groupNestedResult (SP.DiagScript 
                        [SP.Group "g" 
                          [SP.ScriptTestCase _, 
                            SP.Group "h" 
                             [SP.ScriptTestCase _]]]) = True
groupNestedResult  _                                  = False 


         
testLoopExplicit ::  SkriptAssertion
testLoopExplicit parseResult =
  let diagMsg       = DiagnosisMessage 0xA0 0xB0 [0x1,0x2,0x3]
      diagMsgExpect = DiagnosisMessage 0xB0 0xA0 [0xaa,0xbb,0xcc]
      expected      = SP.DiagScript {
        SP.scriptElements = [
          SP.Loop "loopA" 10 [
            SP.ScriptTestCase (
              SP.TestCase "abc" diagMsg diagMsgExpect 2000 0xA0 0xB0) ]]}
      mkTest = either
          (\error -> HU.assertBool ("was not parsed correctly:" ++ show error) False)
          (\x -> expected HU.@=? x) in
  mkTest parseResult



generalAssertion :: ParsedSkript -> (DiagScript -> Bool) -> HU.Assertion 
generalAssertion parseResult checkResult =
  let mkTest = either
         (\error -> HU.assertBool ("was not parsed correctly:" ++ show error) False)                       
         (\x     -> HU.assertBool ("parsed correctly:")    $ checkResult x)   in
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


-- Function for quickly comparing input from .skr file and parsed Output
devTest :: String -> IO ()
devTest f = do
  s <- readFile f
  let p = SP.parseScript s  
  putStrLn s 
  putStrLn $ show p 


current = (scriptPath ++ "/canmsgSimple.skr")
