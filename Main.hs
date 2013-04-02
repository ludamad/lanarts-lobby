-- Loosely based on http://www.catonmat.net/blog/simple-haskell-tcp-server/
-- and https://github.com/chrisdone/hulk/
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

-- While generally I wanted to avoid language extensions, 
-- overloaded strings makes string literals passable as any string type (not just String).
-- The amount of boilerplate this saves was deemed worth it.
{-# LANGUAGE OverloadedStrings #-}

import qualified Network as Net 
import System.IO (hSetBuffering, hClose, BufferMode(..), Handle)

import Control.Concurrent (forkIO, MVar, Chan, writeChan, dupChan, readChan, newChan, killThread)
import Control.Monad (forever, void, liftM)

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Control.Exception as Except
import qualified System.IO.Error as Err
import GHC.IO.Exception (IOErrorType(ResourceVanished), ioe_type)

import qualified Data.ByteString.Lazy as BSL

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import Data.IORef
import qualified System.Posix as Posix

import UserStats
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
            `Except.finally` do { killThread writerThreadID ; hClose clientConn `Except.catch` errHandler; DB.closeDB dbConn }

  where errHandler e 
            | Err.isEOFError e = return ()
            | ioe_type e == ResourceVanished = return ()
            | otherwise = putStrLn $ "UNCAUGHT EXCEPTION " ++ (show e)

-- Send broadcast messages to a single client
-- We listen to the channel for incoming messages.
-- Note channels are one-write interface multiple-read interfaces, so in essence each thread sees its own queue
writerThread :: Configuration -> MessageQueue -> DB.DBConnection -> ClientConnection -> IO ()
writerThread conf msgQueue dbConn clientConn =
    forever $ do
        msg <- readChan msgQueue
        sendMessage clientConn msg

-- Parse and handle messages from a single client
readerThread :: Configuration -> MessageQueue -> DB.DBConnection -> ClientConnection -> IO ()
readerThread conf msgQueue dbConn clientConn = do
    -- Expect a user connection or user creation message first
    user <- authenticateUser =<< recvMessage clientConn
    if user == nullUser then return ()
    else forever $ do
        msg <- recvMessage clientConn
        queueBroadcastMessage msg
        dbStoreMessage dbConn msg

  where queueBroadcastMessage = writeChan msgQueue
        authenticateUser (LoginMessage u p) = do
            maybeUser <- dbAuthenticateUser dbConn u (SHA256.hash $ T.encodeUtf8 p)
            case maybeUser of
                Just user -> do 
                    sendMessage clientConn (ServerMessage "LoginSuccess" (T.concat ["Successfully logged in as ", u, "!"]) )
                    return user
                Nothing -> do
                    sendMessage clientConn (ServerMessage "LoginFailure" "Problem logging in: Bad username or password!")
                    return nullUser
        authenticateUser (CreateUserMessage u p) = do
            maybeUser <- dbCreateUser dbConn u (SHA256.hash $ T.encodeUtf8 p)
            case maybeUser of
                Just user -> do
                    sendMessage clientConn (ServerMessage "LoginSuccess" (T.concat ["Successfully created and logged in as ", u, "!"]) )
                    return user
                Nothing -> do
                    sendMessage clientConn (ServerMessage "LoginFailure" (T.concat ["Problem creating the user ", u, "!"]))
                    return nullUser
        authenticateUser _ = do
            sendMessage clientConn (ServerMessage "ProtocolFailure" "Bad message recieved, expected LoginMessage or CreateUserMessage!")
            return nullUser
