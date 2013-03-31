-- Loosely based on http://www.catonmat.net/blog/simple-haskell-tcp-server/
-- and https://github.com/chrisdone/hulk/
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

import qualified Network as Net 
import System.IO (hSetBuffering, hClose, BufferMode(..), Handle)

import Control.Concurrent (forkIO, MVar, Chan, writeChan, dupChan, readChan, newChan, killThread)
import Control.Monad (forever, void, liftM)

import qualified Control.Exception as Except
import qualified System.IO.Error as Err

import qualified Data.ByteString.Lazy as BSL

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.IORef
import qualified System.Posix as Posix

import Session
import Message
import Configuration
import qualified DBAccess as DB

type ClientConnection = Handle
type MessageQueue = Chan Message

-- For robustness, we should ignore certain signals
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
    -- Create a channel that can be listened to for broadcast messages
    rootMsgQueue <- newIORef =<< newChan 

    forever $ do -- Spawn threads to handle each client
        (clientConn, _, _) <- Net.accept sock

        -- Ensure we don't buffer communications since we want real-time behaviour
        hSetBuffering clientConn NoBuffering

        -- Connect to MongoDB once per client
        dbConn <- DB.connectDB (databaseIP conf)
        msgQueue <- dupChan =<< (readIORef rootMsgQueue) -- dupChan is misleading, essentially we create another listener for the channel
        writeIORef rootMsgQueue msgQueue -- Ensures we don't leak memory! We don't want to hold onto a growing but not consumed channel.

        writerThreadID <- forkIO $ writerThread conf msgQueue dbConn clientConn -- Writer thread
            `Except.catch` errHandler 

        forkIO $ readerThread conf msgQueue dbConn clientConn -- Reader thread
            `Except.catch` errHandler 
            `Except.finally` do { killThread writerThreadID ; hClose clientConn ; DB.closeDB dbConn }

  where errHandler e 
            | Err.isEOFError e = return ()
            | otherwise = putStrLn $ "TEST " ++ (show e)

-- Send broadcast messages to a single client
writerThread :: Configuration -> MessageQueue -> DB.DBConnection -> ClientConnection -> IO ()
writerThread conf msgQueue dbConn clientConn =
    forever $ do
        msg <- readChan msgQueue
        sendMessage clientConn msg

-- Parse and handle messages from a single client
readerThread :: Configuration -> MessageQueue -> DB.DBConnection -> ClientConnection -> IO ()
readerThread conf msgQueue dbConn clientConn =
    forever $ do
        maybeMsg <- recvMessage clientConn
        case maybeMsg of
            Just msg -> do { queueBroadcastMessage msg ; print msg }
            Nothing -> error "Ill-formatted message!!"
  where queueBroadcastMessage = writeChan msgQueue
