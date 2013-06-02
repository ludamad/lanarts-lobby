{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports -fno-warn-name-shadowing #-}

-- While generally I wanted to avoid language extensions, 
-- overloaded strings makes string literals passable as any string type (not just String).
-- The amount of boilerplate this saves was deemed worth it.
{-# LANGUAGE OverloadedStrings #-}

import Network (withSocketsDo)
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
import Control.Applicative

import System.Environment (getArgs)

import qualified DBAccess as DB
import qualified Crypto.Hash.SHA256 as SHA

import UserStats
import Configuration
import Session
import Game
import Message

data AppState = AppState {
    appConf :: Configuration
    , appConnPool :: DB.DBConnectionPool
}

getDBConn :: AppState -> IO DB.DBConnection
getDBConn = DB.getDBConnection . appConnPool

-- For robustness, we should ignore certain signals
installSignalHandlers :: Configuration -> IO ()
installSignalHandlers conf = do
    ignoreSignal Posix.sigPIPE
    if handleSigINT conf then ignoreSignal Posix.sigINT else return ()
  where ignoreSignal pipe = void $ Posix.installHandler pipe Posix.Ignore Nothing

application :: AppState ->  Web.Request -> ResourceT IO Web.Response
application appState request = do
    parseResult <- Web.requestBody request $$ sinkParser (fmap JSON.fromJSON JSON.json)
    liftIO $ putStrLn $ "Request parse result: " ++ (show parseResult)
    case parseResult of
        JSON.Error str -> liftIO $ Except.throwIO (Err.mkIOError Err.userErrorType ("parseResult error in 'application': " ++ str) Nothing Nothing)
        JSON.Success message -> do
            response <- liftIO $ handleMessage appState message
            return $ Web.responseLBS HTTP.status200 [("Content-Type", "application/json")] $ JSON.encode response

loginSuccessMessage :: DB.DBConnection -> T.Text -> IO Message 
loginSuccessMessage dbConn username = do
    session <- dbGetSession dbConn username
    return $ LoginSuccessMessage (sessionId session)

handleLogin :: AppState -> (T.Text, T.Text) -> IO Message
handleLogin appState (username, password) = do
    dbConn <- getDBConn appState
    maybeUser <- dbAuthenticateUser dbConn username (SHA.hash $ T.encodeUtf8 password)
    case maybeUser of
        Just user -> loginSuccessMessage dbConn username
        Nothing -> return $ ServerMessage "LoginFailure" "Incorrect username or password."

handleGuestLogin :: AppState -> T.Text -> IO Message
handleGuestLogin appState username = do
    dbConn <- getDBConn appState
    maybeUser <- dbFindUser dbConn username
    case maybeUser of
        Just user -> return $ ServerMessage "GuestLoginFailure" "Sorry, the username you wish to use is registered."
        Nothing -> LoginSuccessMessage <$> (sessionId <$> dbGetSession dbConn username)

handleCreateUser :: AppState -> (T.Text, T.Text) -> IO Message
handleCreateUser appState (username, password) = do
    dbConn <- getDBConn appState
    maybeUserFind <- dbFindUser dbConn username 
    if maybeUserFind /= Nothing then
        return $ ServerMessage "RegisterFailure" (T.concat ["Unable to create user account '", username, "' because it already exists."])
    else do
        maybeUser <- dbCreateUser dbConn username (SHA.hash $ T.encodeUtf8 password)
        if maybeUser == Nothing then 
            return $ ServerMessage "RegisterFailure" (T.concat ["Unable to create user account '", username, "' due to unexpected database error."])
        else 
            loginSuccessMessage dbConn username

handleCreateGame :: AppState -> (T.Text, T.Text) -> IO Message
handleCreateGame appState (sessionId, ip) = do
    dbConn <- getDBConn appState
    session <- dbGetSessionByID dbConn sessionId
    game <- dbNewGame dbConn (sessionUserName session) ip
    return $ GameCreateSuccessMessage (gameId game)
    
-- TODO: authenticate session
handleJoinGame :: AppState -> (T.Text, T.Text, T.Text) -> IO Message
handleJoinGame appState (username, sessionId, gameId) = do
    dbConn <- getDBConn appState
    dbJoinGame dbConn gameId username
    return $ ServerMessage "JoinSuccess" "You have joined."

handleGameStatus :: AppState -> T.Text -> IO Message
handleGameStatus appState gameId = do
    dbConn <- getDBConn appState
    maybeGame <- dbGetGame dbConn gameId
    case maybeGame of
        Just game -> return $ GameStatusSuccessMessage (GameStatus (gameHostIp game) (gameHost game) (gamePlayers game))
        Nothing -> return $ ServerMessage "NoSuchGame" "The game requested does not exist."


handleMessage :: AppState -> Message -> IO Message
handleMessage appState (LoginMessage u p) = handleLogin appState (u, p)
handleMessage appState (CreateUserMessage u p) = handleCreateUser appState (u, p)
handleMessage appState (CreateGameMessage u s) = handleCreateGame appState (s, "127.0.0.1")
handleMessage appState (JoinGameMessage u s gid) = handleJoinGame appState (u, s, gid)
handleMessage appState (GameStatusRequestMessage gid) = handleGameStatus appState gid
handleMessage _ msg = return msg

setUpDBIndices :: AppState -> IO ()
setUpDBIndices appState = do
    dbConn <- getDBConn appState 
    dbSessionIndexSetup dbConn (sessionTimeOut $ appConf appState)
    dbGameIndexSetup dbConn (staleGameTimeOut $ appConf appState)

main :: IO ()
main = withSocketsDo $ do 
    putStrLn $ "Welcome to LANARTS Lobby Server"
    conf <- fmap Configuration.parseConfiguration getArgs
    installSignalHandlers conf
    putStrLn $ "Connecting to database at " ++ (show (databaseIP conf))
    dbConnPool <- DB.newDBConnectionPool (databaseIP conf) (dbConnections conf) 
    let appState = AppState conf dbConnPool
    let warpSettings = Warp.defaultSettings { Warp.settingsPort = serverPort conf, Warp.settingsHost = Warp.Host $ serverHostAddr conf }
    setUpDBIndices appState
    putStrLn $ "Connected to database"
    Warp.runSettings warpSettings (application appState)
