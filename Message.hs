{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

module Message (
        Message
        ,loginMessageID
        ,createUserMessageID
        ,chatMessageID
        ,getTextOfLen
        ,getText
        ,getMessage
    ) where

-- The Get monad allows one to encapsulate deserialization as a series of actions
import Data.Binary.Get
import Control.Monad

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- Represents a parsed message to be handled by the server
data Message = LoginMessage { username :: T.Text, password :: T.Text }
            | CreateUserMessage { username :: T.Text, password :: T.Text }
            | ChatMessage { message :: T.Text }

loginMessageID = 0
createUserMessageID = 1
chatMessageID = 2

-- Get text of a specified length from a stream
getTextOfLen :: Integral a => a -> Get T.Text
getTextOfLen len = liftM T.decodeUtf8 $ getByteString (fromIntegral len) 

-- First get the text size, and get text of that length from a stream
getText :: Get T.Text
getText = getTextOfLen =<< getWord32be

-- Get a textual pair from a stream
getTextPair :: Get (T.Text, T.Text)
getTextPair  = (liftM2 (,)) getText getText

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
