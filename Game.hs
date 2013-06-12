{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}

-- While generally I wanted to avoid language extensions, 
-- overloaded strings makes string literals passable as any string type (not just String).
-- The amount of boilerplate this saves was deemed worth it.
{-# LANGUAGE OverloadedStrings #-}

module Game where

import qualified Data.Time as Time

import Database.MongoDB ( (=:), Document, select )

import qualified DBAccess as DB
import qualified Data.Text as T

data Game = Game {
        gameId :: T.Text
        , gameHost :: T.Text
        , gameHostIp :: T.Text
        , gamePlayers :: [T.Text] -- Includes hostPlayer
        , gameCreationTime :: Time.UTCTime
        , gameLastUpdate :: Time.UTCTime -- Used to determine if game entries should be kept alive. Hosts can ping server to keep game alive    
}

toDocumentPartial (Game _ hostPlayer ip players creationTime lastUpdate) = [ "hostPlayer" =: hostPlayer, "ip" =: ip, "players" =: players, "creationTime" =: creationTime, "lastUpdate" =: lastUpdate ] 
instance DB.DBStorable Game where
    toDocument game = ("_id" =: DB.textToObjId (gameId game) ) : toDocumentPartial game
    fromDocument doc = Game (DB.objIdToText $ get "_id") (get "hostPlayer") (get "ip") (get "players") (get "creationTime") (get "lastUpdate")
      where get field = DB.docLookUp doc field

dbNewGame :: DB.DBConnection -> T.Text -> T.Text -> IO Game
dbNewGame dbConn hostPlayer ip = do
    time <- Time.getCurrentTime
    let game = (Game "" hostPlayer ip [] time time)
    id <- DB.dbStore dbConn "games" (toDocumentPartial game)
    return $ game { gameId = DB.objIdToText id }

dbGetGame :: DB.DBConnection -> T.Text -> IO (Maybe Game)
dbGetGame dbConn gameId = DB.dbFindVal dbConn "games" [ "_id" =: DB.textToObjId gameId ]

dbGetAllGames :: DB.DBConnection -> IO [Game]
dbGetAllGames dbConn = map DB.fromDocument `fmap` DB.dbFind dbConn (select [] "games" )


dbUpdateGame :: DB.DBConnection -> T.Text -> IO ()
dbUpdateGame dbConn game = DB.dbUpdateVal dbConn "games" game

dbModifyGame :: DB.DBConnection -> T.Text -> Document -> IO ()
dbModifyGame dbConn gameId = DB.dbFindAndUpdateVal dbConn "games" [ "_id" =: DB.textToObjId gameId ]

dbJoinGame :: DB.DBConnection -> T.Text -> T.Text -> IO ()
dbJoinGame dbConn gameId player = do
    dbModifyGame dbConn gameId ["$push" =: ["players" =: player]]
    return ()
    
dbRemoveGame :: DB.DBConnection -> T.Text -> IO ()
dbRemoveGame dbConn gameId = DB.dbDelete dbConn "games" [ "_id" =: DB.textToObjId gameId ]

dbLeaveGame :: DB.DBConnection -> Game -> T.Text -> IO Game
dbLeaveGame dbConn game player = do
    dbModifyGame dbConn (gameId game) ["$pullAll" =: ["players" =: player]]
    return game { gamePlayers = newPlayers }
  where newPlayers = filter (player /=) (gamePlayers game) 

dbGameIndexSetup :: DB.DBConnection -> Int -> IO ()
dbGameIndexSetup dbConn timeOut = do
    --DB.dbSetIndexTimeOut dbConn "games" "lastUpdate" timeOut
    DB.dbEnsureIndex dbConn "games" "hostPlayer"
