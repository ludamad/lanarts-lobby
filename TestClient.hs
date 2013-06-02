{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports -fno-warn-name-shadowing #-}

import System.IO.Error (isEOFError)

import Control.Monad
import Control.Concurrent
import qualified Control.Exception as Except

import Data.Char (ord, chr)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

import qualified Network.HTTP as HTTP
import qualified Network.URI as URI

import qualified Data.Aeson as JSON

import Message
import Configuration
import Session

quiet (Just a) = a
quiet (Nothing) = undefined

toURI = quiet . URI.parseURI

message2request msg = HTTP.Request (toURI "http://localhost:8080/") HTTP.POST headers str 
  where str = (BS.concat . BSL.toChunks) $ JSON.encode msg
        headers = [ HTTP.mkHeader HTTP.HdrContentLength (show (BS.length str)) ]

sendMessage :: Message -> IO Message
sendMessage msg = do
    let request = message2request msg
    result <- HTTP.simpleHTTP request
    rsp2message result
  where 
    rsp2message (Left err) = error $ "HTTP ERROR OCCURRED: " ++ (show err)
    rsp2message (Right response) = do
        let maybeMsg = JSON.decode $ BSL.fromChunks [HTTP.rspBody response]
        case maybeMsg of 
            Nothing -> error $ "INVALID JSON: " ++ (show $ HTTP.rspBody response)
            Just msg -> return msg

data ClientSession = ClientSession { csSession :: T.Text, csUsername :: T.Text } 

handleMessage :: ClientSession -> [String] -> IO ()
handleMessage session ("send":rest) = do
    let msg = ChatMessage { username = (csUsername session), sessId = (csSession session), message = (T.pack $ unwords rest)}
    void $ handleServerResponse (username msg) =<< sendMessage msg
handleMessage session ("game":rest) = do
    let msg = CreateGameMessage { username = (csUsername session), sessId = (csSession session)}
    void $ handleServerResponse (username msg) =<< sendMessage msg
handleMessage session ("join":rest) = do
    let msg = JoinGameMessage { username = (csUsername session), sessId = (csSession session), joinGameId = joinGameId}
    print rest
    void $ handleServerResponse (username msg) =<< sendMessage msg
    where joinGameId = T.pack (head rest)
handleMessage session msg = putStrLn $ "Unrecognized message format for message " ++ (unwords msg)


handleServerResponse :: T.Text -> Message -> IO (Maybe ClientSession)
handleServerResponse username (LoginSuccessMessage sessionId) = do
    putStrLn $ "Authorized with session = " ++ (show sessionId)
    return $ Just $ ClientSession sessionId username
handleServerResponse _  msg = do 
    putStrLn $ "Server sent message: " ++ (show msg )
    return Nothing

handleAuthMessage :: [String] -> IO (Maybe ClientSession)
handleAuthMessage ["create", u, p] = handleServerResponse (T.pack u) =<< sendMessage ( CreateUserMessage (T.pack u) (T.pack p) )
handleAuthMessage ["login", u, p] = handleServerResponse (T.pack u) =<< sendMessage ( LoginMessage (T.pack u) (T.pack p) )
handleAuthMessage _ = return $ Nothing

handleInputL :: ClientSession -> [[String]] -> IO ()
handleInputL session (msg:msgs) = do
    handleMessage session msg
    handleInputL session msgs
handleInputL session [] = return ()
 
handleInput :: [[String]] -> IO ()
handleInput (msg:msgs) = do
    maybeSession <- handleAuthMessage msg
    case maybeSession of 
        Just session -> handleInputL session msgs
        Nothing -> do
            putStrLn $ "Invalid login message '" ++ (unwords msg) ++  "'."
            handleInput msgs
handleInput [] = return () 

main :: IO ()
main = do 
    stdIn <- getContents
    let msgs = map (words) (lines stdIn)
    handleInput msgs
    where errHandler e
            | isEOFError e = return()
            | otherwise  = putStrLn (show e)

