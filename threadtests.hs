import Control.Concurrent
import System.Posix
import Control.Monad

main = do
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

