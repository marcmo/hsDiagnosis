module Com.DiagBase where

import Com.DiagMessage
import Com.HSFZMessage
import Diag.DiagnosisCodes
import System.IO
import Foreign(Word8)
import Control.Monad.Reader
import Prelude hiding (catch,log)

receiveBufSize = 4096 :: Int
pollingMs = 100 :: Int
diagPort = 6801 :: Int

data DiagConnection = MkDiagConnection {
	diagHandle :: Handle,
	chatty :: Bool,
	connectionTimeout :: Int
}
data DiagConfig = MkDiagConfig {
  host :: String,
  port :: Int,
  source :: Word8,
  target :: Word8,
  verbose :: Bool,
  diagTimeout :: Int
} deriving (Show)

type Net = ReaderT DiagConnection IO
type Callback = Maybe DiagnosisMessage -> IO ()

printNegativeResponses xs =
  forM_ xs $ \x ->
    when (isNegativeResponse x) $ do
      let (DiagnosisMessage _ _ (_:_:err:_)) = x
      print err
      print $ "negative response: " ++ nameOfError err

isNegativeResponse (DiagnosisMessage _ _ (x:_)) =
  x == negative_response_identifier
isNegativeResponse _ = False

liftReader a = ReaderT (return . runReader a)

responsePending ::  HSFZMessage -> Bool
responsePending m = let p = diagPayload (hsfz2diag m) in
      length p == 3 && head p == 0x7f && p!!2 == 0x78


-- Convenience.
io :: IO a -> Net a
io = liftIO
log :: String -> Net ()
log s = do
    v <- asks chatty
    when v $ io $ print s


