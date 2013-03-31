{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

import Network (withSocketsDo, connectTo, PortID(..), Socket)
import System.IO (hSetBuffering, hPutStrLn, hClose, BufferMode(..), Handle)
import System.IO.Error (isEOFError)

import qualified Control.Exception as Except

import System.Posix

import Session
import Message
import Configuration

main :: IO ()
main = withSocketsDo $ do
    _ <- installHandler sigPIPE Ignore Nothing
    let conf = Configuration.defaultConfiguration
    handle <- connectTo "localhost" (serverPort conf)
    talkToServer handle `Except.catch` errHandler `Except.finally` hClose handle
    where errHandler e 
            | isEOFError e = return()
            | otherwise  = putStrLn (show e) 

talkToServer :: Handle -> IO ()
talkToServer handle = sendMessage handle (ChatMessage "Hello World!")
