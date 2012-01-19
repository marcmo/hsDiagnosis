import Tests.DiagnoserScriptParserTests
import Tests.HSFZTests
import Tests.ExecuterTests
import Tests.PreProcessorTests
import Test.Framework (defaultMain)


main = do
  scriptTests <- diagnoserScripterTests
  preProTests <- preProcessorTests
  defaultMain [scriptTests,hsfzTests,executerTests,preProTests]

