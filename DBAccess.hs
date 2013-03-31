{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

-- While generally I wanted to avoid language extensions, 
-- overloaded strings makes string literals passable as any string type (not just String).
-- The amount of boilerplate this saves was deemed worth it.
{-# LANGUAGE OverloadedStrings #-} 

module DBAccess (
    DBConnection
    ,connectDB
    ,closeDB
    ,withDBDo
    ,logMessage
    ,printMessages
) where

import Database.MongoDB as MDB

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

logMessage :: DBConnection -> T.Text -> T.Text -> IO (MDB.Value)
logMessage dbConn user msg = do
    time <- Time.getCurrentTime
    withDBDo dbConn $ MDB.insert "messages" [ "user" =: user, "message" =: msg, "time" =: time ]

printMessages :: DBConnection -> IO ()
printMessages dbConn = do
    document <- withDBDo dbConn $ MDB.rest =<< MDB.find ( MDB.select [] ( "messages") )
    print document
