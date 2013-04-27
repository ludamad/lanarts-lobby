{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}

-- While generally I wanted to avoid language extensions, 
-- overloaded strings makes string literals passable as any string type (not just String).
-- The amount of boilerplate this saves was deemed worth it.
{-# LANGUAGE OverloadedStrings #-}

module Game where

import qualified Data.Time as Time

import Database.MongoDB ( (=:) )

import qualified DBAccess as DB
import qualified Data.Text as T

data Game = Game {
        gameId :: T.Text
        , gameHost :: T.Text
        , gamePlayers :: [T.Text] -- Includes host
        , gameCreationTime :: Time.UTCTime
        , gameLastUpdate :: Time.UTCTime -- Used to determine if game entries should be kept alive. Hosts can ping server to keep game alive    
}

toDocumentPartial (Game _ host players creationTime lastUpdate) = [ "host" =: host, "players" =: players, "creationTime" =: creationTime, "lastUpdate" =: lastUpdate ] 

instance DB.DBStorable Game where
    toDocument game = ("gameId" =: DB.textToObjId (gameId game) ) : toDocumentPartial game
    fromDocument doc = Game (DB.objIdToText $ get "_id") (get "host") (get "players") (get "creationTime") (get "lastUpdate")
      where get field = DB.docLookUp doc field

dbNewGame :: DB.DBConnection -> (T.Text, [T.Text]) -> IO Game
dbNewGame dbConn (host, players) = do
    time <- Time.getCurrentTime
    let game = (Game "" host players time time)
    id <- DB.dbStore dbConn "games" (DB.toDocument game)
    return $ game { gameId = DB.objIdToText id }

dbGetGame :: DB.DBConnection -> T.Text -> IO (Maybe Game)
dbGetGame dbConn gameId = DB.dbFindVal dbConn "games" [ "_id" =: DB.textToObjId gameId ]

dbGameIndexSetup :: DB.DBConnection -> Int -> IO ()
dbGameIndexSetup dbConn timeOut = do
    DB.dbSetIndexTimeOut dbConn "games" "lastUpdate" timeOut
    DB.dbEnsureIndex dbConn "games" "host"
