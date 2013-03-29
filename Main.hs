-- Following http://www.catonmat.net/blog/simple-haskell-tcp-server/
-- and https://github.com/chrisdone/hulk/
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

import Network (listenOn, withSocketsDo, accept, Socket, PortID(..))
import System.IO (hSetBuffering, hClose, BufferMode(..), Handle)
import System.IO.Error (isEOFError)
import Control.Concurrent (forkIO)

import qualified Control.Exception as Except

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Posix

import Session
import Message

main :: IO ()
main = withSocketsDo $ do
    _ <- installHandler sigPIPE Ignore Nothing
    sock <- listenOn (PortNumber 6112)
    sockHandler sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    _ <- forkIO (commandProcessor handle `Except.catch` errHandler `Except.finally` hClose handle)
    sockHandler sock -- rinse, repeat
    where errHandler e 
            | isEOFError e = return()
            | otherwise  = putStrLn (show e) 

commandProcessor :: Handle -> IO ()
commandProcessor handle = do
    line <- T.hGetLine handle 
    T.putStrLn line
    commandProcessor handle
