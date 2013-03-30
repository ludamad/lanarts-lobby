-- Loosely based on http://www.catonmat.net/blog/simple-haskell-tcp-server/
-- and https://github.com/chrisdone/hulk/
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

import Network (listenOn, withSocketsDo, accept, Socket)
import System.IO (hSetBuffering, hClose, BufferMode(..), Handle)
import System.IO.Error (isEOFError)
import Control.Concurrent (forkIO)
import Control.Monad (forever)

import qualified Control.Exception as Except

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Posix as Posix

import Session
import Message
import Configuration
import DBAccess

type ClientConnection = Handle

main :: IO ()
main = withSocketsDo $ do
    -- Install posix handler to ignore broken pipes
    _ <- Posix.installHandler Posix.sigPIPE Ignore Nothing

    let conf = defaultConfiguration    
    sock <- listenOn $ serverPort conf 

    forever $ do
        (conn, _, _) <- accept sock
        hSetBuffering conn NoBuffering
        -- Spawn one thread per connection
        forkIO $ onConnect conf conn
            `Except.catch` errHandler 
            `Except.finally` hClose conn
  where errHandler e 
            | isEOFError e = return()
            | otherwise  = putStrLn (show e) 

onConnect :: Configuration -> ClientConnection -> IO ()
onConnect conf conn =
    dbConn <-  
    forever $ do
        line <- T.hGetLine conn 
        T.putStrLn line
