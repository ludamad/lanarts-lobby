-- Following http://www.catonmat.net/blog/simple-haskell-tcp-server/
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

import Network (listenOn, withSocketsDo, accept, PortID(..))
import System.Environment (getArgs)
import System.IO (hSetBuffering, BufferMode(..), Handle)
import Control.Concurrent (forkIO)

import qualified Control.Exception as Except

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Posix

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    _ <- installHandler sigPIPE Ignore Nothing
    let port = fromIntegral (read $ head args :: Int)
    sock <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ (head args)
    sockHandler sock

sockHandler sock = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    _ <- forkIO $ commandProcessor handle
    sockHandler sock

swallowIOErr :: IO () -> IO ()
swallowIOErr operation = Except.catch operation handler
    where handler = (\_ -> return ()) :: IOError -> IO ()

commandProcessor :: Handle -> IO ()
commandProcessor handle = swallowIOErr $ do
    line <- T.hGetLine handle 
    T.putStrLn line
    commandProcessor handle 

