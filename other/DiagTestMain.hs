{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
import Network
import System.IO hiding (hPutStrLn,hPutStr)
import Data.ByteString.Char8(hPutStr)
import qualified Data.ByteString as S
import Control.Concurrent(putMVar,MVar,forkIO,newEmptyMVar,takeMVar,yield)
import Foreign(Ptr,Word8,free,mallocBytes)
import Foreign.C.Types(CChar)
import Control.Exception
import Text.Printf(printf)
import Prelude hiding (catch,log)

receiveBufSize = 4096 :: Int
connectionTimeout = 2000

main = sendMessage "localhost" [0x4,0x3] >>= print

sendMessage :: String -> [Word8] -> IO (Maybe S.ByteString)
sendMessage host msg = bracket connect disconnect loop
  where
    disconnect = hClose
    loop st    = catch (run (S.pack msg) st) (\(_ :: IOException) -> return Nothing)
    connect = notify $ do
        h <- connectTo host (Service "6666")
        hSetBuffering h NoBuffering
        return h
    notify = bracket_ (printf "Connecting to %s ... " host >> hFlush stdout) (putStrLn "done.")

run :: S.ByteString -> Handle -> IO (Maybe S.ByteString)
run msg h = do
    print "running all things..."
    m <- newEmptyMVar
    forkIO $ catch (listenForResponse h m) (\(e :: IOException) -> print e >> putMVar m Nothing >> return ())
    pushOutMessage msg h m
    putStrLn "[S]going on..."
    ss <- takeMVar m 
    return ss
  where
    pushOutMessage :: S.ByteString -> Handle -> MVar (Maybe S.ByteString) -> IO ()
    pushOutMessage msg h m = do
        S.putStrLn "[S]pushing out the message"
        putStrLn ("[S]sending --> " ++ show msg)
        hPutStr h msg
        hFlush h -- Make sure that we send data immediately
        return ()

listenForResponse ::  Handle -> MVar (Maybe S.ByteString) -> IO ()
listenForResponse h m = do  putStrLn "   [R]listening for response..."
                            msg <- receiveResponse h
                            putMVar m msg
                            return ()
  where

    receiveResponse :: Handle -> IO (Maybe S.ByteString)
    receiveResponse h = do
        buf <- mallocBytes receiveBufSize
        dataResp <- receiveMsg buf h
        free buf
        return dataResp

    receiveMsg :: Ptr CChar -> Handle -> IO (Maybe S.ByteString)
    receiveMsg buf h = do
        putStrLn ("   [R]wait for data with timeout:" ++ show connectionTimeout ++ " ms\n")
        dataAvailable <- waitForData h connectionTimeout
        if not dataAvailable then (putStrLn "\n   [R]no message available") >> return Nothing
          else do
            answereBytesRead <- hGetBufNonBlocking h buf receiveBufSize
            Just `fmap` S.packCStringLen (buf,answereBytesRead)

    waitForData ::  Handle -> Int -> IO (Bool)
    waitForData h waitTime_ms = do
      S.putStr "."
      yield
      inputAvailable <- hWaitForInput h 10
      if inputAvailable then return True 
        else if waitTime_ms > 0
              then waitForData h (waitTime_ms - 10)
              else return False
                
