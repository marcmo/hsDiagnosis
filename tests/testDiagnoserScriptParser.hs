import qualified Diagnoser.DiagScriptParser as SP
import Com.DiagMessage
import qualified Test.HUnit as HU
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)


main = do
  loopAssertion <- testLoopIO
  groupAssertion <- testGroupIO
  let tests = [
              testGroup "diagnoser-script Group" [
                      testCase "loop construct" loopAssertion,
                      testCase "group construct" groupAssertion
                  ]
          ]
  defaultMain tests

testGroupIO :: IO HU.Assertion
testGroupIO = do
  s <- readFile "diagnoser/Beispiele_GROUPs/EXAMPLE_nested_groups.skr"
  return $ testGrouping s

testGrouping :: String -> HU.Assertion
testGrouping s = "nyi" HU.@=? "todo"

testLoopIO :: IO HU.Assertion
testLoopIO = readFile "diagnoser/loop.skr" >>= return . testLoop

testLoop :: String -> HU.Assertion
testLoop s =
  let parseResult = SP.parseScript s
      diagMsg = DiagnosisMessage 0xA0 0xB0 [0x1,0x2,0x3]
      diagMsgExpect = DiagnosisMessage 0xB0 0xA0 [0xaa,0xbb,0xcc]
      expected = SP.DiagScript {
        SP.scriptElements = [
          SP.Loop "loopA" 10 [
            SP.ScriptTestCase (
              SP.TestCase "abc" diagMsg diagMsgExpect 2000 0xA0 0xB0) ]]}
      mkTest = either
          (\error -> HU.assertBool ("was not parsed correctly:" ++ show error) False)
          (\x -> expected HU.@=? x) in
  mkTest parseResult


