import DiagClient
import Maybe
import Util
import Control.Monad
import DiagnosticConfig

conf = femConfigA

klemme30bVerkuerzen = sendData conf [0x31,0x01,0x10,0x01,0x10,0x10,0x2a] >> return ()

enable c  = sendData conf [0xbf,0x14,0x01,0x01,c] >> return ()

disable c  = sendData conf [0xbf,0x14,0x01,0x00,c] >> return ()

getWakeupReasons ::  IO [Word8]
getWakeupReasons = do
  resp <- sendData conf [0x22,0xdc,0x59]
  return $ (maybe [] (drop 4 . diagPayload) resp) 

showWakeupReasons ::  IO String
showWakeupReasons = showAsHexString `liftM` getWakeupReasons

disableReasons ::  [Word8] -> IO ()
disableReasons rs = mapM_ disable rs

disableAllReasons = getWakeupReasons >>= disableReasons
