module Com.DiagBase where

import Com.DiagMessage
import Com.HSFZMessage
import Diag.DiagnosisCodes
import Network(PortID(PortNumber),connectTo)
import System.IO
import Control.Concurrent(putMVar,MVar,forkIO,newEmptyMVar,takeMVar)
import Foreign(Ptr,Word8,free,mallocBytes)
import Foreign.C.String(peekCStringLen)
import Foreign.C.Types(CChar)
import Control.Monad.Reader
import Control.Exception
import Text.Printf(printf)
import Util.Encoding(string2hex)
import Maybe(isJust)
import Control.Applicative((<$>))
import Debug.Trace
import Prelude hiding (catch,log)

receiveBufSize = 4096 :: Int
pollingMs = 100 :: Int
diagPort = 6801 :: Int

data DiagConnection = DiagConnection { diagHandle :: Handle, chatty :: Bool }
data DiagConfig = MkDiagConfig {
  host :: String,
  port :: Int,
  source :: Word8,
  target :: Word8,
  verbose :: Bool
} deriving (Show)

type Net = ReaderT DiagConnection IO
type Callback = Maybe DiagnosisMessage -> IO ()

isNegativeResponse Nothing = False
isNegativeResponse (Just (DiagnosisMessage _ _ (x:xs))) =
  x == negative_response_identifier

liftReader a = ReaderT (return . runReader a)

responsePending ::  Maybe HSFZMessage -> Bool
responsePending = 
  maybe False (\m->
    let p = diagPayload (hsfz2diag m) in
      length p == 3 && p!!0 == 0x7f && p!!2 == 0x78)


-- Convenience.
io :: IO a -> Net a
io = liftIO
log ::  (Show a) => a -> Net ()
log s = do
    v <- asks chatty
    when v $ io $ print s
