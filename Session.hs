{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

-- While generally I wanted to avoid language extensions, 
-- overloaded strings makes string literals passable as any string type (not just String).
-- The amount of boilerplate this saves was deemed worth it.
{-# LANGUAGE OverloadedStrings #-}

module Session (
    Session(..)
    , dbGetSession
    , dbSessionIndexSetup
) where

import qualified Data.Time as Time
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

import Database.MongoDB (Document, (=:), cast, Binary(..), ObjectId(..), Val(..) )

import Control.Applicative ( (<$>) )
import UserStats
import qualified DBAccess as DB

-- A guest user does not have a corresponding user entry with their user name.
data Session = Session {
    sessionId :: T.Text
    , sessionUserName :: T.Text
    , sessionLastUpdate :: Time.UTCTime
}

toDocumentPartial (Session _ userName lastUpdate) = [ "userName" =: userName, "lastUpdate" =: lastUpdate ]
instance DB.DBStorable Session where
    toDocument session = ("_id" =: DB.textToObjId (sessionId session) ) : toDocumentPartial session
    fromDocument doc = Session (DB.objIdToText $ get "_id") (get "userName") (get "lastUpdate")
      where get field = DB.docLookUp doc field

dbNewSession :: DB.DBConnection -> T.Text -> IO Session
dbNewSession dbConn userName = do
    session <- Session "" userName <$> Time.getCurrentTime
    sessId <- DB.dbStore dbConn "sessions" (DB.toDocument session)
    return $ session { sessionId = (DB.objIdToText sessId) }  

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

dbSessionIndexSetup :: DB.DBConnection -> Int -> IO () 
dbSessionIndexSetup dbConn timeOut = do
    DB.dbSetIndexTimeOut dbConn "sessions" "lastUpdate" timeOut
    DB.dbEnsureIndex dbConn "sessions" "userName"
