{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import DiagClient
import Network.Socket
import Script.ErrorMemory
import Script.LoggingFramework

data Sample = Diagsend {message :: [Int]}
            | ReadDtc { dtcKind :: Int }
            | Logging { option :: Int }
              deriving (Show, Data, Typeable)

diagSend ::  Mode Sample
diagSend = mode $ Diagsend {message = def &= typ "number of processors"}
dtc ::  Mode Sample
dtc = mode $ ReadDtc { dtcKind = 1} &= text "read DTCs in ecu (primary = 1, secondary = 2)"
logging :: Mode Sample
logging = mode $ Logging { option = 1}

main ::  IO ()
main = withSocketsDo $ do 
  actions <- cmdArgs "DiagnosisTool 0.1.0, (c) Oliver Mueller 2010" modes
  execute actions

execute :: Sample -> IO ()
execute (Diagsend m) = putStrLn $ "send: " ++ show m
execute (ReadDtc x)
  | x == 1 = print "readPrimaryErrorMemory" -- readPrimaryErrorMemory
  | x == 2 = print "readSecondaryErrorMemory" -- readSecondaryErrorMemory
execute (Logging x) 
  | x == 1  = enable
  | x == 2  = showMapping

execute2 (ReadPrimaryDtcs s o) = print "Create was called..."
execute2 (Diff old new o) = print "Create was called..."

data DTCHandling = ReadPrimaryDtcs {src :: FilePath, out :: FilePath}
                 | Diff {old :: FilePath, new :: FilePath, out :: FilePath}
                   deriving (Data,Typeable,Show,Eq)

outFlags = text "Output file" & typFile

create = mode $ ReadPrimaryDtcs
    {src = "." &= text "Source directory" & typDir
    ,out = "ls.txt" &= outFlags
    } &= prog "diagtool" & text "read error memory"

diff = mode $ Diff
    {old = def &= typ "OLDFILE" & argPos 0
    ,new = def &= typ "NEWFILE" & argPos 1
    ,out = "diff.txt" &= outFlags
    } &= text "Perform a diff"

-- modes = [create,diff]
modes = [diagSend,dtc,logging]
