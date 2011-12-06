import Tests.DiagnoserScriptParserTests
import Tests.HSFZTests
import Test.Framework (defaultMain)

main = do
  scriptTests <- diagnoserScripterTests
  defaultMain [scriptTests,hsfzTests]

