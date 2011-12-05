import qualified Diagnoser.DiagScriptParser as SP
import Com.DiagMessage
import qualified Test.HUnit as HU
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Text.Parsec.Error
import Diagnoser.DiagScriptParser

main = do
  loopAssertion          <- generalTest testLoopFull          "tests/diagnoser/implemented/loopSimple.skr"
  loopNestedAssertion    <- assertionTest loopNestedResult    "tests/diagnoser/implemented/loopNested.skr"
  groupNestedAssertion   <- assertionTest groupNestedResult   "tests/diagnoser/implemented/groupNested.skr"
  let tests = [
              testGroup "diagnoser-script Group" [
                        testGroup "loops"
                                  [testCase "simple loop construct with detailed test" loopAssertion,
                                   testCase "nested loop construct" loopNestedAssertion],
                        testGroup "groups"
                                  [testCase "groupSimple construct" groupNestedAssertion]
        ]]
  defaultMain tests

type ParsedSkript    = Either ParseError SP.DiagScript
type SkriptAssertion = ParsedSkript -> HU.Assertion 
--data SkriptTest      = SkriptTest SkriptAssertion FilePath



loopNestedResult :: DiagScript -> Bool
loopNestedResult (SP.DiagScript 
                   [SP.Loop "loopA" 10 
                     [SP.ScriptTestCase _,
                       SP.Loop "loopB" 10 
                         [SP.ScriptTestCase _]]]) = True
loopNestedResult  _                               = False 



groupNestedResult :: DiagScript -> Bool
groupNestedResult (SP.DiagScript 
                        [SP.Group "g" 
                          [SP.ScriptTestCase _, 
                            SP.Group "h" 
                             [SP.ScriptTestCase _]]]) = True
groupNestedResult  _                                  = False 


         
testLoopFull ::  SkriptAssertion
testLoopFull parseResult =
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


