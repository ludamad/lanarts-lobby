{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

-- While generally I wanted to avoid language extensions, 
-- overloaded strings makes string literals passable as any string type (not just String).
-- The amount of boilerplate this saves was deemed worth it.
{-# LANGUAGE OverloadedStrings #-}

import qualified Network.Wai as Web
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.Aeson as JSON

import qualified Control.Exception as Except
import qualified System.Posix as Posix
import qualified System.IO.Error as Err

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Conduit.Attoparsec (sinkParser)

import qualified Data.Conduit.List as CL
import Data.Conduit (ResourceT, ($$), Sink)

import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void) 
import System.Environment (getArgs)

import qualified DBAccess as DB
import Configuration
import Message

data AppState = AppState {
    appConf :: Configuration
    , appConnPool :: DB.DBConnectionPool
}

-- For robustness, we should ignore certain signals
installSignalHandlers :: Configuration -> IO ()
installSignalHandlers conf = do
    ignoreSignal Posix.sigPIPE
    if handleSigINT conf then ignoreSignal Posix.sigINT else return ()
  where ignoreSignal pipe = void $ Posix.installHandler pipe Posix.Ignore Nothing

application :: AppState ->  Web.Request -> ResourceT IO Web.Response
application appState request = do
    parseResult <- Web.requestBody request $$ sinkParser (fmap JSON.fromJSON JSON.json)
    message <- liftIO $ handleMessage parseResult
    return $ Web.responseLBS HTTP.status200 [("Content-Type", "application/json")] $ JSON.encode message

handleMessage :: JSON.Result Message -> IO Message
handleMessage (JSON.Error str) = Except.throwIO (Err.mkIOError Err.userErrorType str Nothing Nothing) 
handleMessage (JSON.Success msg) = return msg

setUpDB :: AppState -> IO ()
setUpDB appState = do
    conn <- DB.getDBConnection $ appConnPool appState
    DB.dbEval conn evalString 
  where
    timeOut = sessionTimeOut (appConf appState) 
    evalString = T.concat [ "db.sessions.ensureIndex({ \"userID\": 1, \"status\": 1 }, { \"expireAfterSeconds\": ", T.pack (show timeOut)," } )" ]

main :: IO ()
main = do 
    conf <- fmap Configuration.parseConfiguration getArgs
    installSignalHandlers conf
    connPool <- DB.newDBConnectionPool (databaseIP conf) (dbConnections conf) 
    let appState = AppState conf connPool
    let warpSettings = Warp.defaultSettings { Warp.settingsPort = serverPort conf, Warp.settingsHost = Warp.Host $ serverHostAddr conf }
    setUpDB appState
    Warp.runSettings warpSettings (application appState)
