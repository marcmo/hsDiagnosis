module Diag.Web.SessionSupport where

import Data.IORef
import Data.Maybe (fromJust,isJust)
import Control.Monad

type Env = IORef [(String, IORef String)]

nullEnv :: IO Env
nullEnv = newIORef []

sessionEnv ::  IO Env
sessionEnv = nullEnv

printEnv :: Env -> IO ()
printEnv envRef = do
  print "Environment:"
  env <- readIORef envRef
  s <- mapM showBinding env
  print s
    where showBinding (a,b) = do
            sb <- readIORef b
            return $ show (a,sb)

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . isJust . lookup var
getVar :: Env -> String -> IO (Maybe String)
getVar envRef var  =  do env <- readIORef envRef
                         maybe (return Nothing)
                               (readIORef >=> return . Just)
                               (lookup var env)
setVar :: Env -> String -> String -> IO Bool
setVar envRef var value = do env <- readIORef envRef
                             maybe (return False)
                                   (\s -> writeIORef s value >> return True)
                                   (lookup var env)

defineVar :: Env -> String -> String -> IO String
defineVar envRef var value = do
    alreadyDefined <- isBound envRef var
    if alreadyDefined
       then setVar envRef var value >> return value
       else do
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef ((var, valueRef) : env)
          return value
