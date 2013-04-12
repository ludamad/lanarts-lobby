{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

module Configuration where

data Configuration = Configuration {
        databaseIP :: String
        , serverHostAddr :: String
        , serverPort :: Int
        , handleSigINT :: Bool
        , dbConnections :: Int
        , sessionTimeOut :: Int -- in seconds
}

parseConfiguration :: [String] -> Configuration
parseConfiguration args = Configuration { 
        databaseIP =
            if length args > 2 then args !! 2 else "localhost"
        , serverHostAddr = 
            if length args > 1 then args !! 1 else "0.0.0.0"
        , serverPort = 
            if length args > 0 then read (args !! 0) else (8080::Int)
        , handleSigINT = False -- Should be true before deploying
        , dbConnections = 10
        , sessionTimeOut = 60*3 -- session is held for 3 minutes by default
    }
