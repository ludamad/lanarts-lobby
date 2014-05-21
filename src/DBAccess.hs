-- Contains helper functions for dealing with MongoDB
-- Especially tries to make the somewhat complex query format easier to use

{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

-- While generally I wanted to avoid language extensions, 
-- overloaded strings makes string literals passable as any string type (not just String).
-- The amount of boilerplate this saves was deemed worth it.
{-# LANGUAGE OverloadedStrings #-} 
-- FlexibleInstances + UndecidableInstances allows us to detect types that are both FromJSON and ToJSON and make them DBStorable.
{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

module DBAccess (
    DBConnection
    , textToObjId
    , objIdToText
    , DBStorable 
    , DBConnectionPool
    , connectDB
    , newDBConnectionPool
    , getDBConnection
    , closeDB
    , docLookUp
    , dbEval
    , dbStore
    , dbSetIndexTimeOut
    , dbStoreVal
    , dbUpdate
    , dbUpdateVal
    , dbDelete
    , dbFind
    , dbFindOne
    , dbFindVal
    , dbFindAndUpdateVal
    , dbFindTakeN
    , dbFindTakeNSortBy
    , dbEnsureIndex
    , prependTimeStamp
    , toDocument
    , fromDocument
    , fromBinary
    , withDBDo
    , printMessages
) where

import Database.MongoDB as MDB
import Database.MongoDB.Admin as MDB

import qualified Contrib.AesonBson as AB
import qualified Data.Aeson as JSON

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Time as Time
import qualified Control.Exception as Except

import qualified System.IO.Error as Err
import qualified System.IO.Pool as P

import Numeric (readHex, showHex)
import Control.Monad (liftM, void)

class DBStorable a where
   -- Convert an arbitrary value into a MongoDB object that we can store
   toDocument :: a -> Document
   -- Convert a MongoDB object to an arbitrary value
   -- Throw exception if not possible
   fromDocument :: Document -> a

instance (JSON.ToJSON a, JSON.FromJSON a) => DBStorable a where
    toDocument = valueToDoc . JSON.toJSON
      where valueToDoc (JSON.Object v) = AB.bsonify v
            valueToDoc _ = error "DBAccess.dbStore API error: requires object value" -- Partial functions are icky and unhaskelly, but there's no better way
    fromDocument doc = result2maybe $ JSON.fromJSON $ JSON.Object (AB.aesonify doc)
      where result2maybe (JSON.Error str) = Except.throw (Err.mkIOError Err.userErrorType str Nothing Nothing)
            result2maybe (JSON.Success v) = v

type DBConnection = MDB.Pipe
type DBConnectionPool = P.Pool Err.IOError DBConnection

textToObjId :: T.Text -> MDB.ObjectId
textToObjId text = MDB.Oid (takeNum $ readHex start) (takeNum $ readHex end)
  where str = T.unpack text
        (start, end) = (take 8 str, drop 8 str)
        takeNum [(num, _)] = num
        takeNum list = error $ "DBAccess.hs: Invalid textToObjId string '" ++ str ++ "'!" ++ (show list)

objIdToText :: MDB.ObjectId -> T.Text
objIdToText (Oid w32 w64) = T.pack (showHex w32 (showHex w64 "") )

newDBConnectionPool :: String -> Int -> IO DBConnectionPool 
newDBConnectionPool hostName amount = P.newPool connFactory amount
  where connFactory = P.Factory { P.newResource = MDB.connect $ MDB.host hostName, P.killResource = MDB.close, P.isExpired = MDB.isClosed }

getDBConnection :: DBConnectionPool -> IO DBConnection
getDBConnection = MDB.runIOE . P.aResource

connectDB :: T.Text -> IO DBConnection
connectDB hostName = MDB.runIOE $ MDB.connect $ MDB.host (T.unpack hostName)

closeDB :: DBConnection -> IO ()
closeDB = MDB.close

-- Turn a failed operation into a custom thrown IOException
throwOnDBFail :: Either MDB.Failure a -> IO a
throwOnDBFail (Left _) = Except.throwIO (Err.mkIOError Err.userErrorType "Database error! TODO: better message" Nothing Nothing)
throwOnDBFail (Right a) = return a
 
withDBDo :: DBConnection -> Action IO a -> IO a
withDBDo dbConn dbAction = do 
  -- NB: MongoDB allows for multiple databases, but we just need one (we call it 'lanarts-lobby').
    result <- MDB.access dbConn MDB.master "lanarts-lobby" dbAction
    throwOnDBFail result 

unwrapObjectId :: MDB.Value -> MDB.ObjectId
unwrapObjectId (ObjId objId) = objId
unwrapObjectId _ = error "unwarpObjectId: failure" 

-- Store a MongoDB object in the collection 'collection'
dbStore :: DBConnection -> T.Text -> MDB.Document -> IO MDB.ObjectId
dbStore dbConn collection document = unwrapObjectId `fmap` (withDBDo dbConn $ MDB.insert collection document)

-- Store a DBStorable using dbStore
dbStoreVal :: DBStorable a => DBConnection -> T.Text -> a -> IO MDB.ObjectId
dbStoreVal dbConn collection = dbStore dbConn collection . toDocument

---- Find a field in a MongoDB object, or error if not found
docLookUp :: MDB.Val a => MDB.Document -> T.Text -> a
docLookUp document field = errIfNotExist $ MDB.lookup field document
  where errIfNotExist (Just a) = a
        errIfNotExist Nothing = error ("Field " ++ (show field) ++ " not found or wrong type in " ++ (show document))

fromBinary :: MDB.Binary -> BS.ByteString
fromBinary (MDB.Binary str) = str

-- Takes a MongoDB object and adds a time-stamp field
-- Note that MongoDB objects (Document) are represented as lists of fields
prependTimeStamp :: MDB.Document -> IO Document
prependTimeStamp doc = do
    time <- Time.getCurrentTime
    return $ ("timestamp" =: time) : doc

-- Updates the object with the matching '_id' field, or inserts it if it does not exist or find a match.
dbUpdate :: DBConnection -> T.Text -> MDB.Document -> IO () 
dbUpdate dbConn collection document = withDBDo dbConn $ MDB.save collection document

-- Update an object, it should convert to an object with an '_id' field.
dbUpdateVal :: DBStorable a => DBConnection -> T.Text -> a -> IO () 
dbUpdateVal dbConn collection = dbUpdate dbConn collection . toDocument

-- Delete an object
dbDelete :: DBConnection -> T.Text -> MDB.Document -> IO ()
dbDelete dbConn collection document = withDBDo dbConn $ MDB.delete (MDB.select document collection)

-- Returns an instance from 'collection' matching the given partial object 'document'.
-- Efficient if we know there is only one such object.
dbFindOne :: DBConnection -> T.Text -> MDB.Document -> IO (Maybe MDB.Document)
dbFindOne dbConn collection document = withDBDo dbConn $ MDB.findOne (MDB.select document collection)

-- Like dbFindOne but converts storable type
dbFindVal :: DBStorable a => DBConnection -> T.Text -> MDB.Document -> IO (Maybe a)
dbFindVal dbConn collection document = do
    doc <- dbFindOne dbConn collection document
    return $ fromDocument `fmap` doc

-- Atomic find and update operation
dbFindAndUpdateVal :: DBConnection -> T.Text -> MDB.Document -> MDB.Document -> IO ()
dbFindAndUpdateVal dbConn collection document operation = withDBDo dbConn $ MDB.modify (MDB.select document collection) operation

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

dbEval :: DBConnection -> T.Text -> IO ()
dbEval dbConn str = void $ (withDBDo dbConn $ MDB.eval $ MDB.Javascript [] str :: IO Bool)

-- Ugly, but pragmatic implementation.
-- Currently this is not directly supported by the MongoDB Haskell driver.
dbSetIndexTimeOut :: DBConnection -> T.Text -> T.Text -> Int -> IO ()
dbSetIndexTimeOut dbConn collection field timeOut = dbEval dbConn $ evalStr
    where evalStr = T.concat [ "db.", collection, ".ensureIndex({ \"", field, "\": 1 }, { \"expireAfterSeconds\": ", T.pack (show timeOut)," } );" ]

dbEnsureIndex :: DBConnection -> T.Text -> T.Text -> IO () 
dbEnsureIndex dbConn collection field = withDBDo dbConn $ MDB.ensureIndex $ index collection [ field =: (1::Int)]

printMessages :: DBConnection -> IO ()
printMessages dbConn = do
    document <- withDBDo dbConn $ MDB.rest =<< MDB.find ( MDB.select [] ( "messages") )
    print document
