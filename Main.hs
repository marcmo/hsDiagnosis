module Main () where

import System(getArgs)
import DiagClient
import Data.Maybe(isJust,fromJust)
import Util

femConfigA = MkDiagConfig "10.40.39.33" "6801" "f5" "40"
conf = femConfigA

main = do 
  print "hi"
  r <- sendData conf [0x22,0xF1,0x90]
  case r of 
    (Just res) -> print res
    Nothing -> putStrLn "no response"
  print "hi"
  -- -- contents <- getContents
  -- (f:out:_) <- getArgs
  -- contents <- readFile f
  -- case parseNumbers contents of 
  --   Left err -> do  putStrLn "Error parsing input:"
  --                   print err
  --   -- Right r -> writeFile out $ process r
  --   Right r -> putStrLn $ process r
