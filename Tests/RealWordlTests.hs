
import Diagnoser.TestCaseExecuter
import qualified Diagnoser.DiagScriptParser as SP

dem1 = "Tests/realWorldSkripts/dem1.skr"
dem1FullNames = "Tests/realWorldSkripts/dem1FullNames.skr"

example f = do
  s <- readFile f
  let p = SP.parseScript s
  return p


runExample file   = do
  parsed <-  example file
  let (Right p) = parsed
  runScript p


runDem1 = runExample dem1FullNames
