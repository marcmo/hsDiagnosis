import qualified Diagnoser.DiagScriptParser as SP
import Com.DiagMessage
import qualified Test.HUnit as HU
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Text.Parsec.Error
import Diagnoser.DiagScriptParser

main = do
  testCaseAssertion        <- assertionTest testCaseExplicitResult  "tests/diagnoser/implemented/diagExplicit.skr"
  loopAssertion            <- generalTest   testLoopExplicit        "tests/diagnoser/implemented/loopSimple.skr"
  loopNestedAssertion      <- assertionTest loopNestedResult        "tests/diagnoser/implemented/loopNested.skr"
  groupNestedAssertion     <- assertionTest groupNestedResult       "tests/diagnoser/implemented/groupNested.skr"
  groupNumberNameAssertion <- assertionTest groupNumberNameResult   "tests/diagnoser/implemented/groupNumberName.skr"
  waitSimpleAssertion      <- assertionTest waitSimpleResult        "tests/diagnoser/implemented/waitSimple.skr"
  let tests = [
              testGroup "diagnoser-script Group" [
                        testGroup "testCase (DIAG)"
                                  [testCase "testCase (DIAG) construct (test written expcicitly)"  testCaseAssertion],
                        testGroup "loops"
                                  [testCase "simple loop construct (test written expcicitly)" loopAssertion,
                                   testCase "nested loop construct" loopNestedAssertion],
                        testGroup "groups"
                                  [testCase "nested group construct" groupNestedAssertion,
                                   testCase "simple group construct with a number as name" groupNumberNameAssertion],
                        testGroup "wait"
                                  [testCase "simple wait construct" waitSimpleAssertion]
        ]]
  defaultMain tests

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
