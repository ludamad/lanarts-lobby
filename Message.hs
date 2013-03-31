{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

module Message (
        Message
        ,loginMessageID
        ,createUserMessageID
        ,chatMessageID
        ,putText
        ,putMessage
        ,getTextOfLen
        ,getText
        ,getMessage
    ) where

-- The Get monad allows one to encapsulate deserialization as a series of actions
import Data.Binary.Get
-- The Put monad allows one to encapsulate serialization as a series of actions
import Data.Binary.Put
import Control.Monad
import Data.ByteString as BS

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- Represents a parsed message to be handled by the server
data Message = LoginMessage { username :: T.Text, password :: T.Text }
            | CreateUserMessage { username :: T.Text, password :: T.Text }
            | ChatMessage { message :: T.Text }

loginMessageID = 0
createUserMessageID = 1
chatMessageID = 2

-- Put text onto a stream
putText :: T.Text -> Put 
putText text = do
    let byteString = T.encodeUtf8 text
    putWord32be $ fromIntegral $ BS.length byteString
    putByteString byteString

-- Get text of a specified length from a stream
getTextOfLen :: Integral a => a -> Get T.Text
getTextOfLen len = liftM T.decodeUtf8 $ getByteString (fromIntegral len) 

-- First get the text size, and get text of that length from a stream
getText :: Get T.Text
getText = getTextOfLen =<< getWord32be

-- Get a textual pair from a stream
getTextPair :: Get (T.Text, T.Text)
getTextPair  = (liftM2 (,)) getText getText

-- Put a message onto a stream
putMessage :: Message -> Put
putMessage (LoginMessage u p) = putWord32be loginMessageID >> putText u >> putText p
putMessage (CreateUserMessage u p) = putWord32be createUserMessageID >> putText u >> putText p
putMessage (ChatMessage msg) = putWord32be chatMessageID >> putText msg

-- Get a parsed message from a stream
getMessage :: Get Message
getMessage = do
    msgType <- getWord32be
    if msgType == loginMessageID then do
            (user, pass) <- getTextPair
            return LoginMessage { username = user, password = pass }
    else if msgType == createUserMessageID then do
            (user, pass) <- getTextPair
            return CreateUserMessage { username = user, password = pass }
    else if msgType == chatMessageID then do
            msg <- getText
            return ChatMessage { message = msg }
    else error $ "Unknown message type: " ++ (show msgType)

