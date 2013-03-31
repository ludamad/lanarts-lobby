{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

-- While generally I wanted to avoid language extensions, 
-- overloaded strings makes string literals passable as any string type (not just String).
-- The amount of boilerplate this saves was deemed worth it.
{-# LANGUAGE OverloadedStrings #-}

module Message (
        Message(..)
        ,sendMessage
        ,recvMessage
    ) where

import Data.Word
-- The Get monad allows one to encapsulate deserialization as a series of actions
import Data.Binary.Get
-- The Put monad allows one to encapsulate serialization as a series of actions
import Data.Binary.Put
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
-- Easy JSON conversion is supported by the Aeson library
import Data.Aeson as JSON

import qualified Control.Exception as Except
import qualified System.IO.Error as Err
import System.IO (Handle)
import Control.Monad
import Control.Applicative

import qualified Data.Text as T

-- Represents a parsed message to be handled by the server
data Message = LoginMessage { username :: T.Text, password :: T.Text }
            | CreateUserMessage { username :: T.Text, password :: T.Text }
            | ChatMessage { message :: T.Text } deriving (Show)

instance JSON.FromJSON Message where
    parseJSON (JSON.Object jsObject) = do 
        msgType <- jsObject .: "type"
        case msgType :: T.Text of
                "LoginMessage" -> LoginMessage <$> jsObject .: "username" <*> jsObject .: "password"
                "CreateUserMessage" -> CreateUserMessage <$> jsObject .: "username" <*> jsObject .: "password"
                "ChatMessage" -> ChatMessage <$> jsObject .: "message"
                _ -> mzero -- Fail
    parseJSON _ = mzero -- Fail

instance JSON.ToJSON Message where
     toJSON (LoginMessage u p) = JSON.object ["type" .= ("LoginMessage" :: T.Text), "username" .= u, "password" .= p]
     toJSON (CreateUserMessage u p) = JSON.object ["type" .= ("CreateUserMessage" :: T.Text), "username" .= u, "password" .= p]
     toJSON (ChatMessage msg) = JSON.object ["type" .= ("ChatMessage" :: T.Text), "message" .= msg]

putMessage :: Message -> Put
putMessage msg = do
    let byteString = JSON.encode msg
    putWord32be $ fromIntegral (BSL.length byteString)
    putLazyByteString byteString

-- Put a message onto a stream
sendMessage :: Handle -> Message -> IO ()
sendMessage handle msg = BSL.hPut handle $ runPut $ putMessage msg

-- Grabs a certain number of bytes, coerced into a lazy string for use with Aeson
recvBSL :: Handle -> Int -> IO BSL.ByteString
recvBSL handle size = do
    strictString <- BS.hGet handle size
    if BS.length strictString < size then
        Except.throwIO $ Err.mkIOError Err.eofErrorType "Messages.hs: Handle was closed." Nothing Nothing
    else return $ BSL.fromChunks [strictString]

-- Grabs the first 4 bytes to get the message size from a stream
recvMessageSize :: Handle -> IO (Word32)
recvMessageSize handle = liftM (runGet getWord32be) $ recvBSL handle 4

-- Get a message from a stream
recvMessage :: Handle -> IO (Maybe Message) 
recvMessage handle = do
    size <- recvMessageSize handle
    liftM JSON.decode $ recvBSL handle (fromIntegral size)

