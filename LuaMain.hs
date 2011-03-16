import LuaTester(executeLuaScript)
import System.Environment(getArgs)
import System.Directory(doesFileExist)

main = do
  script:_ <- getArgs
  exists <- doesFileExist script
  if exists
  	then executeLuaScript script
  	else error $ "file not found: " ++ script

