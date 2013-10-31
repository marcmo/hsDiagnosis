module Diag.Util.Timer

where

import System.CPUTime
import Text.Printf

timeItMsg :: String -> IO a -> IO a
timeItMsg msg ioa = do
  t1 <- getCPUTime
  a <- ioa
  t2 <- getCPUTime
  let t :: Double
      t = fromIntegral (t2-t1) * 1e-12
  printf "CPU time for %s: %6.5fs\n" msg t
  return a

