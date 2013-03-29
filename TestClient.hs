-- Following http://www.catonmat.net/blog/simple-haskell-tcp-server/
-- and https://github.com/chrisdone/hulk/
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

import Network (withSocketsDo, connectTo, PortID(..), Socket)
import System.IO (hSetBuffering, hPutStrLn, hClose, BufferMode(..), Handle)
import System.IO.Error (isEOFError)

import qualified Control.Exception as Except

import System.Posix

import Session
import Message

main :: IO ()
main = withSocketsDo $ do
    _ <- installHandler sigPIPE Ignore Nothing
    handle <- connectTo "localhost" (PortNumber 6112)
    talkToServer handle `Except.catch` errHandler `Except.finally` hClose handle
    where errHandler e 
            | isEOFError e = return()
            | otherwise  = putStrLn (show e) 

talkToServer :: Handle -> IO ()
talkToServer handle = hPutStrLn handle "Hello World!"
