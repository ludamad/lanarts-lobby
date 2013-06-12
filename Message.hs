{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

-- While generally I wanted to avoid language extensions, 
-- overloaded strings makes string literals passable as any string type (not just String).
-- The amount of boilerplate this saves was deemed worth it.
{-# LANGUAGE OverloadedStrings #-}

module Message (
        Message(..)
        ,dbStoreMessage
        ,GameStatus(..)
        ,utcToSeconds
    ) where

-- Easy JSON conversion is supported by the Aeson library
import Data.Aeson as JSON

import Data.Time as Time
import Data.Time.LocalTime ( timeOfDayToTime )

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
            | CreateGameMessage { username :: T.Text, sessId :: T.Text }
            | JoinGameMessage { username :: T.Text, sessId :: T.Text, joinGameId :: T.Text }
            | RemoveGameMessage { username :: T.Text, sessId :: T.Text, removeGameId :: T.Text }
            | ChatMessage { username :: T.Text, sessId :: T.Text, message :: T.Text }
            | GameStatusRequestMessage { gid :: T.Text }
            | GameListRequestMessage
-- Server responses
            | LoginSuccessMessage { sessId :: T.Text }
            | GameCreateSuccessMessage { newGameId :: T.Text }
            | GameStatusSuccessMessage { gameStatus :: GameStatus }
            | GameListSuccessMessage { gameList :: [GameStatus]}
            | ServerMessage { context :: T.Text, message :: T.Text } -- Primarily used for error messages
                deriving (Show)

instance JSON.FromJSON Message where
    parseJSON (JSON.Object jsObject) = do 
        msgType <- jsObject .: "type"
        case msgType :: T.Text of
-- User requests
                "LoginMessage" -> LoginMessage <$> jsObject .: "username" <*> jsObject .: "password"
                "GuestLoginMessage" -> GuestLoginMessage <$> jsObject .: "username"
                "CreateUserMessage" -> CreateUserMessage <$> jsObject .: "username" <*> jsObject .: "password"
                "CreateGameMessage" -> CreateGameMessage <$> jsObject .: "username" <*> jsObject .: "sessionId"
                "JoinGameMessage" -> JoinGameMessage <$> jsObject .: "username" <*> jsObject .: "sessionId" <*> jsObject .: "gameId"
                "RemoveGameMessage" -> RemoveGameMessage <$> jsObject .: "username" <*> jsObject .: "sessionId" <*> jsObject .: "gameId"
                "ChatMessage" -> ChatMessage <$> jsObject .: "username" <*>  jsObject .: "sessionId" <*> jsObject .: "message"
                "GameStatusRequestMessage" -> GameStatusRequestMessage <$> jsObject .: "gameId"
                "GameListRequestMessage" -> return GameListRequestMessage
-- Server responses
                "LoginSuccessMessage" -> LoginSuccessMessage <$> jsObject .: "sessionId"
                "GameCreateSuccessMessage" -> GameCreateSuccessMessage <$> jsObject .: "gameId"
                "GameStatusSuccessMessage" -> GameStatusSuccessMessage <$> jsObject .: "gameStatus"
                "GameListSuccessMessage" -> GameListSuccessMessage <$> jsObject .: "gameList"
                "ServerMessage" -> ServerMessage <$> jsObject .: "context" <*> jsObject .: "message"
                _ -> mzero -- Fail
    parseJSON _ = mzero -- Fail

instance JSON.ToJSON Message where
-- User requests
    toJSON (LoginMessage u p) = JSON.object ["type" .= ("LoginMessage" :: T.Text), "username" .= u, "password" .= p]
    toJSON (GuestLoginMessage u) = JSON.object ["type" .= ("GuestLoginMessage" :: T.Text), "username" .= u]
    toJSON (CreateUserMessage u p) = JSON.object ["type" .= ("CreateUserMessage" :: T.Text), "username" .= u, "password" .= p]
    toJSON (CreateGameMessage u s) = JSON.object ["type" .=  ("CreateGameMessage" :: T.Text), "username" .= u, "sessionId" .= s]
    toJSON (JoinGameMessage u s gid) = JSON.object ["type" .= ("JoinGameMessage" :: T.Text), "username" .= u, "sessionId" .= s, "gameId" .= gid]
    toJSON (RemoveGameMessage u s gid) = JSON.object ["type" .= ("RemoveGameMessage" :: T.Text), "username" .= u, "sessionId" .= s, "gameId" .= gid]
    toJSON (ChatMessage u sid msg) = JSON.object ["type" .= ("ChatMessage" :: T.Text), "username" .= u, "sessionId" .= sid, "message" .= msg]
    toJSON (GameStatusRequestMessage gid) = JSON.object ["type" .= ("GameStatusRequestMessage" :: T.Text), "gameId" .= gid]
    toJSON GameListRequestMessage = JSON.object ["type" .= ("GameListRequestMessage" :: T.Text)]
-- Server responses
    toJSON (LoginSuccessMessage sid) = JSON.object ["type" .= ("LoginSuccessMessage" :: T.Text), "sessionId" .= sid]
    toJSON (GameCreateSuccessMessage gid) = JSON.object ["type" .= ("GameCreateSuccessMessage" :: T.Text), "gameId" .= gid]
    toJSON (GameStatusSuccessMessage gstatus) = JSON.object [ "type" .= ("GameStatusSuccessMessage" :: T.Text), "gameStatus" .= JSON.toJSON gstatus]
    toJSON (GameListSuccessMessage games) = JSON.object ["type" .= ("GameListSuccessMessage" :: T.Text), "gameList" .= map JSON.toJSON games]
    toJSON (ServerMessage cntxt msg) = JSON.object ["type" .= ("ServerMessage" :: T.Text), "context" .= cntxt, "message" .= msg]

utcToSeconds :: Time.UTCTime -> Int
utcToSeconds = sec . utcToLocalTime utc
    where sec (LocalTime (ModifiedJulianDay day) tod) = fromIntegral $ day * 60 * 60 * 24 + (truncate $ timeOfDayToTime tod)

dbStoreMessage :: DB.DBConnection -> Message -> IO ()
dbStoreMessage dbConn msg = do
    doc <- DB.prependTimeStamp $ DB.toDocument msg
    void $ DB.dbStore dbConn "messages" doc

data GameStatus = GameStatus { gsId :: T.Text, gsHostIP :: T.Text, gsHostPlayer :: T.Text, gsPlayerList :: [T.Text], gsCreationTime :: Int }
    deriving (Show)

instance JSON.ToJSON GameStatus where
    toJSON (GameStatus id ip host players time) = JSON.object ["id" .= id, "ip" .= ip, "host" .= host, "players" .= players, "creationTime" .= time]

instance JSON.FromJSON GameStatus where
    parseJSON (JSON.Object jsObject) = GameStatus <$> jsObject .: "id" <*> jsObject .: "ip" <*> jsObject .: "host" <*> jsObject .: "players" <*> jsObject .: "creationTime"
