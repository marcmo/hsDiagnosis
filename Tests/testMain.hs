import Tests.DiagnoserScriptParserTests
import Tests.HSFZTests
import Tests.ExecuterTests
import Test.Framework (defaultMain)

main = do
  scriptTests <- diagnoserScripterTests
  defaultMain [scriptTests,hsfzTests,executerTests]

