{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

-- While generally I wanted to avoid language extensions, 
-- overloaded strings makes string literals passable as any string type (not just String).
-- The amount of boilerplate this saves was deemed worth it.
{-# LANGUAGE OverloadedStrings #-}

module UserStats (
    toDocument
    , nullUser
    , fromDocument
    , dbAuthenticateUser
    , dbUpdateUser
    , dbFindUser
    , dbCreateUser
    , dbPrintUsers
) where

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified DBAccess as DB

import Database.MongoDB (Document, (=:), cast, Binary(..), ObjectId(..) )

type SHA256Hash = BS.ByteString

data User = User {
    userId :: ObjectId
    , username :: T.Text
    , passwordHash :: SHA256Hash
} deriving (Show, Eq)

-- Represents an invalid user. We could use a Maybe type to represent this, but it would be tedious in the common case.
nullUser :: User
nullUser = User { userId = Oid 0 0, username = "", passwordHash = "" } 

-- Find a user with the given username
dbFindUser :: DB.DBConnection -> T.Text -> IO (Maybe User)
dbFindUser dbConn name = do
    maybeUser <- DB.dbFindOne dbConn "users" [ "username" =: name ]
    case maybeUser of 
        Just user -> return $ Just (fromDocument user)
        Nothing -> return Nothing 

-- Find a user with the given username
dbPrintUsers :: DB.DBConnection -> IO ()
dbPrintUsers dbConn = do
    users <- DB.dbFindTakeN dbConn "users" [] 20
    print users

-- Find a user with the given username & password
dbAuthenticateUser :: DB.DBConnection -> T.Text -> SHA256Hash -> IO (Maybe User)
dbAuthenticateUser dbConn name passHash = do
    maybeUser <- dbFindUser dbConn name
    case maybeUser of
        Just user -> return $ if passwordHash user == passHash then Just user else Nothing
        Nothing -> return Nothing
 
-- Convert to a MongoDB object. Includes everything except the ObjectId,
-- because we must not pass an ObjectId on initial creation.
toDocumentPartial :: User -> Document
toDocumentPartial user = [ "username" =: username user, "passwordHash" =: Binary (passwordHash user) ]

-- Convert to a MongoDB object. Includes ObjectId.
toDocument :: User -> Document
toDocument user = ("_id" =: userId user) : (toDocumentPartial user) 

-- Convert from a MongoDB object.
fromDocument :: Document -> User
fromDocument document = User {
    userId = document `DB.docLookUp` "_id"
    , username = document `DB.docLookUp` "username"
    , passwordHash = DB.fromBinary $ document `DB.docLookUp` "passwordHash"
}

-- Update the user with the given ObjectId.
dbUpdateUser :: DB.DBConnection -> User -> IO ()
dbUpdateUser dbConn user = DB.dbUpdate dbConn "users" (toDocument user)

-- Create a user, get assigned a ObjectId.
-- Returns Nothing on failure.
dbCreateUser :: DB.DBConnection -> T.Text -> SHA256Hash -> IO (Maybe User)
dbCreateUser dbConn u p = do
    let partialUser = User { userId = Oid 0 0, username = u, passwordHash = p }
    doc <- DB.prependTimeStamp $ toDocumentPartial partialUser
    _ <- DB.dbStore dbConn "users" doc
    dbAuthenticateUser dbConn u p
