{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

-- While generally I wanted to avoid language extensions, 
-- overloaded strings makes string literals passable as any string type (not just String).
-- The amount of boilerplate this saves was deemed worth it.
{-# LANGUAGE OverloadedStrings #-}

module Session (
    SessionId
    , Session(..)
    , dbGetSession
) where

import qualified Data.Time as Time
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

import Numeric (readHex, showHex)

import Database.MongoDB (Document, (=:), cast, Binary(..), ObjectId(..), Val(..) )

import UserStats
import qualified DBAccess as DB

type SessionId = T.Text 

-- A guest user does not have a corresponding user entry with their user name.
data Session = Session {
    sessionId :: T.Text
    , sessionUserName :: T.Text
    , sessionLastUpdate :: Time.UTCTime
}

instance DB.DBStorable Session where
    toDocument (Session sessId userName lastUpdate) = [ "_id" =: sessionToObjId sessId, "userName" =: userName, "lastUpdate" =: lastUpdate ]
    fromDocument doc = Session (objToSessionId $ get "_id") (get "userName") (get "lastUpdate")
      where get field = DB.docLookUp doc field

sessionToObjId :: SessionId -> ObjectId
sessionToObjId session = Oid (takeNum $ readHex start) (takeNum $ readHex end)
  where str = show session
        (start, end) = (take 2 str, drop 2 str)
        takeNum [(num, _)] = num
        takeNum _ = error $ "Session.hs: Invalid sessionToObjId string '" ++ str ++ "'!"

objToSessionId :: ObjectId -> SessionId
objToSessionId (Oid w32 w64) = T.pack (showHex w32 (showHex w64 "") ) 

dbNewSession :: DB.DBConnection -> T.Text -> IO Session
dbNewSession dbConn userName = do
    timeStamp <- Time.getCurrentTime
    sessId <- DB.dbStore dbConn "sessions" [ "userName" =: userName, "lastUpdate" =: timeStamp ]
    return $ Session (objToSessionId sessId) userName timeStamp

dbGetSession :: DB.DBConnection -> T.Text -> IO Session
dbGetSession dbConn userName = do
    maybeSession <- DB.dbFindVal dbConn "sessions" [ "userName" =: userName ]
    case maybeSession of 
        Just session -> do 
            timeStamp <- Time.getCurrentTime
            let updatedSession = session { sessionLastUpdate = timeStamp }
            DB.dbUpdateVal dbConn "sessions" updatedSession
            return updatedSession
        Nothing -> dbNewSession dbConn userName
