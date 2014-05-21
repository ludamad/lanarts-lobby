{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

-- While generally I wanted to avoid language extensions, 
-- overloaded strings makes string literals passable as any string type (not just String).
-- The amount of boilerplate this saves was deemed worth it.
{-# LANGUAGE OverloadedStrings #-}

module UserStats (
    User(..)
    , dbAuthenticateUser
    , dbFindUser
    , dbCreateUser
    , dbPrintUsers
) where

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified DBAccess as DB
import qualified Data.Time as Time

import Database.MongoDB (Document, (=:), cast, Binary(..), ObjectId(..) )

type SHA256Hash = BS.ByteString

data User = User {
    userId :: ObjectId
    , username :: T.Text
    , passwordHash :: SHA256Hash
    , creationTime :: Time.UTCTime
} deriving (Show, Eq)
 
-- Convert to a MongoDB object. Includes everything except the ObjectId,
-- because we must not pass an ObjectId on initial creation.
toDocumentPartial :: User -> Document
toDocumentPartial user = [ "username" =: username user, "passwordHash" =: Binary (passwordHash user), "creationTime" =: creationTime user ]

instance DB.DBStorable User where 
    -- Convert to a MongoDB object. Includes ObjectId.
    toDocument user = ("_id" =: userId user) : (toDocumentPartial user) 
    -- Convert from a MongoDB object.
    fromDocument doc = User (get "_id") (get "username") (DB.fromBinary $ get "passwordHash") (get "creationTime")
      where get field = DB.docLookUp doc field

-- Find a user with the given username
dbFindUser :: DB.DBConnection -> T.Text -> IO (Maybe User)
dbFindUser dbConn name = do
    maybeDoc <- DB.dbFindOne dbConn "users" [ "username" =: name ]
    return $ DB.fromDocument `fmap` maybeDoc

dbPrintUsers :: DB.DBConnection -> IO ()
dbPrintUsers dbConn = do
    users <- DB.dbFindTakeN dbConn "users" [] 20
    print users

-- Find a user with the given username & password
dbAuthenticateUser :: DB.DBConnection -> T.Text -> SHA256Hash -> IO (Maybe User)
dbAuthenticateUser dbConn name passHash = do
    maybeUser <- dbFindUser dbConn name
    return $ authenticate =<< maybeUser
  where authenticate user = if passwordHash user == passHash then Just user else Nothing

-- Create a user, get assigned a ObjectId.
-- Returns Nothing on failure.
dbCreateUser :: DB.DBConnection -> T.Text -> SHA256Hash -> IO (Maybe User)
dbCreateUser dbConn u p = do
    time <- Time.getCurrentTime
    let partialUser = User { userId = Oid 0 0, username = u, passwordHash = p, creationTime = time }
    _ <- DB.dbStore dbConn "users" (toDocumentPartial partialUser)
    dbAuthenticateUser dbConn u p
