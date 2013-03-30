{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

module DBAccess (
    DBConnection
    ,connectDB
    ,withDBDo
) where

import Database.MongoDB as MDB
import Data.Text as T

type DBConnection = MDB.Pipe

connectDB :: IO DBConnection
connectDB = MDB.runIOE $ MDB.connect $ host "localhost"

withDBDo :: DBConnection -> T.Text -> Action IO a -> IO (Either Failure a)
withDBDo dbConn dbName = MDB.access dbConn MDB.master dbName

