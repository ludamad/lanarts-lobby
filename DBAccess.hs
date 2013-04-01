{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

-- While generally I wanted to avoid language extensions, 
-- overloaded strings makes string literals passable as any string type (not just String).
-- The amount of boilerplate this saves was deemed worth it.
{-# LANGUAGE OverloadedStrings #-} 

module DBAccess (
    DBConnection
    , connectDB
    , closeDB
    , dbStore
    , dbFind
    , dbFindOne
    , dbFindTakeN
    , dbFindTakeNSortBy
    , prependTimeStamp
    , toDocument
    , fromDocument
    , withDBDo
    , printMessages
) where

import Database.MongoDB as MDB

import qualified Contrib.AesonBson as AB
import qualified Data.Aeson as JSON

import Data.Text as T
import Data.Text.Encoding as T
import Data.Time as Time
import Control.Exception as Except
import System.IO.Error as Err
import Control.Monad(liftM)

type DBConnection = MDB.Pipe

connectDB :: T.Text -> IO DBConnection
connectDB hostName = MDB.runIOE $ MDB.connect $ host (T.unpack hostName)

closeDB :: DBConnection -> IO ()
closeDB = MDB.close

throwOnDBFail :: Either MDB.Failure a -> IO a
throwOnDBFail (Left _) = Except.throwIO (mkIOError Err.userErrorType "Database error! TODO: better message" Nothing Nothing)
throwOnDBFail (Right a) = return a
 
withDBDo :: DBConnection -> Action IO a -> IO a
withDBDo dbConn dbAction = do 
  -- NB: MongoDB allows for multiple databases, but we just need one (we call it 'lanarts-lobby').
    result <- MDB.access dbConn MDB.master "lanarts-lobby" dbAction
    throwOnDBFail result 

-- Store a MongoDB object in the collection 'collection'
dbStore :: DBConnection -> T.Text -> MDB.Document -> IO (MDB.Value)
dbStore dbConn collection document = withDBDo dbConn $ MDB.insert collection document

-- Convert an arbitrary value into a MongoDB object that we can store
-- It must implement the ToJSON typeclass
-- This relies on a helper library for converting Aeson's between format and MongoDB's format for JSON
toDocument :: JSON.ToJSON value => value -> MDB.Document
toDocument = valueToDoc . JSON.toJSON
  where valueToDoc (JSON.Object v) = AB.bsonify v
        valueToDoc _ = error "DBAccess.dbStore API error: requires object value" -- Partial functions are icky and unhaskelly, but there's no better way

-- Convert a MongoDB object to an arbitrary value
-- It must implement the FromJSON typeclass
-- This relies on a helper library for converting between Aeson's format and MongoDB's format for JSON
fromDocument :: JSON.FromJSON value => MDB.Document -> JSON.Result value
fromDocument doc = JSON.fromJSON $ JSON.Object (AB.aesonify doc)

-- Takes a MongoDB object and adds a time-stamp field
-- Note that MongoDB objects (Document) are represented as lists of fields
prependTimeStamp :: MDB.Document -> IO Document
prependTimeStamp doc = do
    time <- Time.getCurrentTime
    return $ ("timestamp" =: time) : doc

-- Returns an instance from 'collection' matching the given partial object 'document'.
-- Efficient if we know there is only one such object.
dbFindOne :: DBConnection -> T.Text -> MDB.Document -> IO (Maybe MDB.Document)
dbFindOne dbConn collection document = withDBDo dbConn $ MDB.findOne (MDB.select document collection)

-- Returns all instances matching a given query.
-- See http://hackage.haskell.org/packages/archive/mongoDB/1.3.2/doc/html/Database-MongoDB-Query.html#t:Query
-- for the valid fields that can go in a query.
dbFind :: DBConnection -> MDB.Query -> IO [MDB.Document]
dbFind dbConn query = withDBDo dbConn $ MDB.rest =<< MDB.find query

-- Returns up to 'num' instances from 'collection' matching the given partial object 'document'.
dbFindTakeN :: DBConnection -> T.Text -> MDB.Document -> Int -> IO [MDB.Document]
dbFindTakeN dbConn collection document num = dbFind dbConn $ allQuery { limit = fromIntegral num }
    where allQuery = MDB.select document collection

-- Returns up to 'num' instances from 'collection' matching the given partial object 'document'.
-- These instances are ordered in ascending order by 'sortField'
dbFindTakeNSortBy :: DBConnection -> T.Text -> MDB.Document -> Int -> T.Text -> IO [MDB.Document]
dbFindTakeNSortBy dbConn collection document num sortField = dbFind dbConn $ allQuery { limit = fromIntegral num, sort = [ sortField =: (1::Int)] }
    where allQuery = MDB.select document collection

printMessages :: DBConnection -> IO ()
printMessages dbConn = do
    document <- withDBDo dbConn $ MDB.rest =<< MDB.find ( MDB.select [] ( "messages") )
    print document
