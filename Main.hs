-- Loosely based on http://www.catonmat.net/blog/simple-haskell-tcp-server/
-- and https://github.com/chrisdone/hulk/
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

import qualified Network as Net 
import System.IO (hSetBuffering, hClose, BufferMode(..), Handle)

import Control.Concurrent (forkIO)
import Control.Monad (forever)

import qualified Control.Exception as Except
import qualified System.IO.Error as Err

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified System.Posix as Posix

import Session
import Message
import Configuration
import qualified DBAccess as DB

type ClientConnection = Handle

installSignalHandlers :: Configuration -> IO ()
installSignalHandlers conf = do
    ignoreSignal Posix.sigPIPE
    if handleSigINT conf then ignoreSignal Posix.sigINT else return ()
  where ignoreSignal pipe = do { _ <- Posix.installHandler pipe Posix.Ignore Nothing ; return () }
 
main :: IO ()
main = Net.withSocketsDo $ do
    let conf = Configuration.defaultConfiguration    
    installSignalHandlers conf
    -- Accept connections on the configured name
    sock <- Net.listenOn $ serverPort conf
    forever $ do -- Spawn threads to handle each client
        (clientConn, _, _) <- Net.accept sock
        -- Ensure we don't buffer communications since we want real-time behaviour
        hSetBuffering clientConn NoBuffering
        -- Connect to MongoDB once per client
        dbConn <- DB.connectDB (databaseIP conf)
        forkIO $ onConnect conf dbConn clientConn
            `Except.catch` errHandler 
            `Except.finally` do { hClose clientConn ; DB.closeDB dbConn }
  where errHandler e 
            | Err.isEOFError e = return()
            | otherwise  = putStrLn (show e) 

-- Handles IO with a single client
onConnect :: Configuration -> DB.DBConnection -> ClientConnection -> IO ()
onConnect conf dbConn clientConn =
    forever $ do
        line <- T.hGetLine clientConn
        _ <- DB.logMessage dbConn (T.pack "User") line 
        _ <- DB.printMessages dbConn
        T.putStrLn line
