import Control.Concurrent
import System.Posix
import Control.Concurrent (threadDelay)
import Control.Monad

main = do
    print "hi..."
    threadDelay (1000*1000*1)
    print "ho..."
    box <- newEmptyMVar
    putMVar box "hello"
    forkIO (echo box)
    putMVar box "hello2"
    print "end"

echo :: MVar String -> IO ()
echo c =  forever $ do
    sleep 1
    print "hi"
    sleep 1
    print "hi"
    msg <- takeMVar c
    print "hi"
    sleep 1
    print msg
    print "hi"

