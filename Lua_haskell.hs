import qualified Scripting.Lua as Lua
import Data.Word
import Com.DiagClient(sendData,diagPayload,Word8)
import Script.LoggingFramework
import DiagnosticConfig(conf)
import Util.Encoding
import Data.Char
import Control.Concurrent(threadDelay)
import Numeric
import Foreign.C
import Foreign.Ptr
import Control.Monad

dofile :: Lua.LuaState -> String -> IO Int
dofile l name = do
    res <- Lua.loadfile l name
    Lua.pcall l 0 0 0
    return res

dostring :: Lua.LuaState -> (Int,Int) -> String -> IO Int
dostring l (params,returns) str = do
    res <- Lua.loadstring l str ""
    Lua.pcall l 0 params returns
    return res

main = do
    l <- Lua.newstate
    Lua.openlibs l
 
    Lua.registerhsfunction l "send" hsSend
    Lua.registerhsfunction l "sleep" hsSleep
    Lua.registerhsfunction l "showMapping" hsLoggingShow

    dofile l "Lua/base.lua"
    dofile l "Lua/script_send_diag.lua"

    dostring l (1,1) "return ip_address"
    ipPresent <- Lua.isnil l (-1)
    if ipPresent
      then print "no ip_address defined"
      else do
          ip <- Lua.tostring l (-1)
          print ip
    Lua.pop l 1
    d <- Lua.gettop l
    print $ "top: " ++ show d
    Lua.close l

string2hex ::  String -> Word8
string2hex = fst . head . readHex

hsSend :: String -> IO String
hsSend xs = do
    let msgx = map (int2Word8 . ord) xs
    maybeResp <- sendData conf msgx
    let res =  maybe ("error occured! no response arrived")
                (\resp-> convertToString (diagPayload resp))
                maybeResp
    putStrLn $ "response in haskell to send back to lua was:" ++ res
    return res

convertToString :: [Word8] -> String
convertToString xs = map (chr . word8ToInt) xs

hsSleep :: Int -> IO ()
hsSleep n = threadDelay(1000*1000*n)

hsLoggingShow :: IO ()
hsLoggingShow = showMapping

