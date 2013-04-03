{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

module Configuration where

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Network.Socket as NetSock

data Configuration = Configuration {
        databaseIP :: T.Text
        , serverHostAddr :: NetSock.HostName
        , serverPort :: NetSock.PortNumber
        , handleSigINT :: Bool
}

getConfiguration :: IO Configuration
getConfiguration = do
    args <- getArgs
    let port = if length args > 0 then read (args !! 0) else (6112::Int)
    let hostAddr = if length args > 1 then args !! 1 else "0.0.0.0"
    let dbIP = if length args > 2 then args !! 2 else "localhost"
    return Configuration { 
        databaseIP = T.pack dbIP
        , serverHostAddr = hostAddr
        , serverPort = NetSock.PortNum . fromIntegral . NetSock.PortNum . fromIntegral $ port -- Get around a 'widely considered misdesigned' byteswap
        , handleSigINT = False -- Should be true before deploying
    }
