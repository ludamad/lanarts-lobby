-- Loosely based on http://www.catonmat.net/blog/simple-haskell-tcp-server/
-- and https://github.com/chrisdone/hulk/
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

import qualified Network as Net 
import System.IO (hSetBuffering, hClose, BufferMode(..), Handle)

import Control.Concurrent (forkIO, MVar, Chan, writeChan, dupChan, readChan, newChan, killThread)
import Control.Monad (forever, void, liftM)

import qualified Control.Exception as Except
import qualified System.IO.Error as Err

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.IORef
import qualified System.Posix as Posix

import Session
import Message
import Configuration
import qualified DBAccess as DB

type ClientConnection = Handle
type MessageQueue = Chan T.Text

installSignalHandlers :: Configuration -> IO ()
installSignalHandlers conf = do
    ignoreSignal Posix.sigPIPE
    if handleSigINT conf then ignoreSignal Posix.sigINT else return ()
  where ignoreSignal pipe = void $ Posix.installHandler pipe Posix.Ignore Nothing

main :: IO ()
main = Net.withSocketsDo $ do
    let conf = Configuration.defaultConfiguration    
    installSignalHandlers conf
    -- Accept connections on the configured name
    sock <- Net.listenOn $ serverPort conf
    rootMsgQueue <- newIORef =<< newChan
    forever $ do -- Spawn threads to handle each client
        (clientConn, _, _) <- Net.accept sock
        -- Ensure we don't buffer communications since we want real-time behaviour
        hSetBuffering clientConn NoBuffering
        -- Connect to MongoDB once per client
        dbConn <- DB.connectDB (databaseIP conf)
        -- Each client has a list of messages that are awaiting sending
        msgQueue <- dupChan =<< (readIORef rootMsgQueue)
        writeIORef rootMsgQueue msgQueue -- Ensures we don't leak memory! We don't want to hold onto a growing but not consumed channel.
        writerThreadID <- forkIO $ writerThread conf msgQueue dbConn clientConn
            `Except.catch` errHandler 
        forkIO $ readerThread conf msgQueue dbConn clientConn
            `Except.catch` errHandler 
            `Except.finally` do { killThread writerThreadID ; hClose clientConn ; DB.closeDB dbConn }
  where errHandler e 
            | Err.isEOFError e = return ()
            | otherwise = putStrLn (show e) 

writerThread :: Configuration -> MessageQueue -> DB.DBConnection -> ClientConnection -> IO ()
writerThread conf msgQueue dbConn clientConn =
    forever $ do
        msg <- readChan msgQueue
        T.hPutStrLn clientConn msg

readerThread :: Configuration -> MessageQueue -> DB.DBConnection -> ClientConnection -> IO ()
readerThread conf msgQueue dbConn clientConn =
    forever $ do
        line <- T.hGetLine clientConn
        _ <- DB.logMessage dbConn (T.pack "User") line
        queueBroadcastMessage line
        T.putStrLn line
  where queueBroadcastMessage = writeChan msgQueue
