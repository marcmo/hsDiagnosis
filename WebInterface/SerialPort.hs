-- | Default serial Port is "/dev/ttyUSB0", specify otherwise in in "config.cfg" as e.g. 'serialPort = "/dev/ttyUSB0"'
module WebInterface.SerialPort (serialSocket) where

import Config
import Data.Maybe(fromMaybe, fromJust)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever)
import Network.WebSockets as WS
import System.Hardware.Serialport
import Control.Exception (IOException, handle)


initSerialPort :: IO SerialPort
initSerialPort = do
    defaultSerialPort <- liftIO $ valueToString $ Config.defaultConfig >>= lookupValue "serialPort"
    let defaultSerialPath = fromMaybe "/dev/ttyUSB0" defaultSerialPort
    serialHandler <- handle ex $ do
                         serial <- openSerial defaultSerialPath defaultSerialSettings
                         return (Just serial)
    return $ fromJust serialHandler
    where  ex :: IOException -> IO (Maybe a)
           ex e = print e >> return Nothing

serialSocketLoop :: SerialPort -> WS.Sink WS.Hybi00 -> IO ()
serialSocketLoop serial sink = do
     serialIn <- liftIO $ recv serial 3
     WS.sendSink sink $  WS.textData  serialIn

serialSocket ::  WS.Request -> WS.WebSockets WS.Hybi00 ()
serialSocket rq = do
    liftIO $ print "serialSocket.........."
    serial <- liftIO initSerialPort
    WS.acceptRequest rq
    sink <- WS.getSink
    forever $ liftIO $ serialSocketLoop serial sink





