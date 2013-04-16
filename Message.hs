{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

-- While generally I wanted to avoid language extensions, 
-- overloaded strings makes string literals passable as any string type (not just String).
-- The amount of boilerplate this saves was deemed worth it.
{-# LANGUAGE OverloadedStrings #-}

module Message (
        Message(..)
        ,dbStoreMessage
    ) where

-- Easy JSON conversion is supported by the Aeson library
import Data.Aeson as JSON

import Control.Monad
import Control.Applicative

import Session

import qualified DBAccess as DB
import qualified Data.Text as T

-- Represents a parsed message to be handled by the server
data Message =
-- User requests
            LoginMessage { username :: T.Text, password :: T.Text }
            | GuestLoginMessage { username :: T.Text } -- If username isn't taken, create login session with no credentials
            | CreateUserMessage { username :: T.Text, password :: T.Text }
            | ChatMessage { username :: T.Text, sessId :: SessionId, message :: T.Text }
-- Server responses
            | LoginSuccessMessage { sessId :: SessionId }
            | ServerMessage { context :: T.Text, message :: T.Text } -- Primarily used for error messages
                deriving (Show)

instance JSON.FromJSON Message where
    parseJSON (JSON.Object jsObject) = do 
        msgType <- jsObject .: "type"
        case msgType :: T.Text of
                "LoginMessage" -> LoginMessage <$> jsObject .: "username" <*> jsObject .: "password"
                "GuestLoginMessage" -> GuestLoginMessage <$> jsObject .: "username"
                "CreateUserMessage" -> CreateUserMessage <$> jsObject .: "username" <*> jsObject .: "password"
                "ChatMessage" -> ChatMessage <$> jsObject .: "username" <*>  jsObject .: "sessionId" <*> jsObject .: "message"
                "LoginSuccessMessage" -> LoginSuccessMessage <$> jsObject .: "sessionId"
                "ServerMessage" -> ServerMessage <$> jsObject .: "context" <*> jsObject .: "message"
                _ -> mzero -- Fail
    parseJSON _ = mzero -- Fail

instance JSON.ToJSON Message where
     toJSON (LoginMessage u p) = JSON.object ["type" .= ("LoginMessage" :: T.Text), "username" .= u, "password" .= p]
     toJSON (GuestLoginMessage u) = JSON.object ["type" .= ("GuestLoginMessage" :: T.Text), "username" .= u]
     toJSON (CreateUserMessage u p) = JSON.object ["type" .= ("CreateUserMessage" :: T.Text), "username" .= u, "password" .= p]
     toJSON (ChatMessage u sid msg) = JSON.object ["type" .= ("ChatMessage" :: T.Text), "username" .= u, "sessionId" .= sid, "message" .= msg]
     toJSON (LoginSuccessMessage sid) = JSON.object ["type" .= ("LoginSuccessMessage" :: T.Text), "sessionId" .= sid]
     toJSON (ServerMessage cntxt msg) = JSON.object ["type" .= ("ServerMessage" :: T.Text), "context" .= cntxt, "message" .= msg]

dbStoreMessage :: DB.DBConnection -> Message -> IO ()
dbStoreMessage dbConn msg = do
    doc <- DB.prependTimeStamp $ DB.toDocument msg
    void $ DB.dbStore dbConn "messages" doc
