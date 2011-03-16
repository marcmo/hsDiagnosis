module LuaTester where

import qualified Scripting.Lua as Lua
import Data.Word
import Data.List(intercalate)
import Com.DiagClient(sendData,diagPayload,DiagConfig(MkDiagConfig))
import DiagnosticConfig(standardDiagTimeout)
import Script.LoggingFramework
import Script.ErrorMemory
import Util.Encoding
import Data.Char
import Control.Concurrent(threadDelay)
import Numeric
import Foreign.C
import Foreign.Ptr
import Control.Monad
import Data.List.Split

lua_noerrors = 0
lua_yield	= 1
lua_errrun = 2
lua_errsyntax = 3
lua_errmem = 4


dofile :: Lua.LuaState -> String -> IO Int
dofile s name = do
    -- (putStrLn $ "called scripter with: " ++ name) >> return 0
    res <- Lua.loadfile s name
    if (res == lua_noerrors)
    	then print $ "loaded file correctly:" ++ name
      else reportError $ "error while loading file: " ++ name
    let handlePcall x
          | x == lua_noerrors = print $ "executed file correctly:" ++ name
          | x == lua_errrun = reportError "run-error"
          | x == lua_errsyntax = reportError "syntax-error"
          | x == lua_errmem = reportError "memory-allocation-error"
          | otherwise = reportError ("unknown error(code " ++ show x ++ ") when executing file:" ++ name)
    Lua.pcall s 0 0 0 >>= handlePcall 
    return res
      where reportError desc = do
                                  err <- Lua.tostring s (-1)
                                  Lua.pop s 1 -- remove error message
                                  error $ desc ++ " - " ++ err

dostring :: Lua.LuaState -> (Int,Int) -> String -> IO Int
dostring s (params,returns) str = do
    res <- Lua.loadstring s str ""
    Lua.pcall s 0 params returns
    return res

executeLuaScript script = do
    s <- Lua.newstate
    Lua.openlibs s
 
    Lua.registerhsfunction s "send" hsSend
    Lua.registerhsfunction s "wait" hsSleep
    Lua.registerhsfunction s "showMapping" hsLoggingShow
    Lua.registerhsfunction s "showPrimaryDtcs" hsGetPrimaryDtcs
    Lua.registerhsfunction s "showSecondaryDtcs" hsGetSecondaryDtcs

    dofile s script
    Lua.close s

string2hex ::  String -> Word8
string2hex = fst . head . readHex

hsSend :: String -> Int -> Int -> Int -> String -> IO String
hsSend ip src target timeout xs = do
    -- putStrLn $ "ip was:" ++ ip2 ++ " hsSend from " ++ show src ++ " to " ++ show target ++ " (timeout=" ++ show timeout ++ ")"
    let m = map (\x->"0x"++x) $ splitOn "," xs
    -- putStrLn $ "message was:" ++ show m
    -- let msgx = map (int2Word8 . ord) xs
    let msgx = map (int2Word8 . read) m
    let conf = MkDiagConfig ip 6801 (fromIntegral src) (fromIntegral target) True timeout
    maybeResp <- sendData conf msgx
    let res =  maybe ("error occured! no response arrived")
                (\resp-> convertToString (diagPayload resp))
                maybeResp
    -- putStrLn $ "response in haskell to send back to lua was:" ++ res
    return res

convertToString :: [Word8] -> String
convertToString xs = intercalate "," ys
    where ys = map ((flip showHex "") . word8ToInt) xs

hsSleep :: Int -> IO ()
hsSleep n = threadDelay(1*1000*n)

hsLoggingShow :: IO ()
hsLoggingShow = showMapping

hsGetPrimaryDtcs :: String -> Int -> Int -> IO ()
hsGetPrimaryDtcs ip src target = readPrimaryErrorMemory conf
    where conf = MkDiagConfig ip 6801 (fromIntegral src) (fromIntegral target) False standardDiagTimeout
  
hsGetSecondaryDtcs :: String -> Int -> Int -> IO ()
hsGetSecondaryDtcs ip src target = readSecondaryErrorMemory conf
    where conf = MkDiagConfig ip 6801 (fromIntegral src) (fromIntegral target) False standardDiagTimeout
  

