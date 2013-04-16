{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

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

data ClientSession = ClientSession { csSession :: SessionId, csUsername :: T.Text } 

handleMessage :: ClientSession -> [String] -> Maybe Message 
handleMessage session ("send":rest) = Just $ ChatMessage { username = (csUsername session), sessId = (csSession session), message = (T.pack $ unwords rest)}  
handleMessage session _ = Nothing

handleAuthResponse :: T.Text -> Message -> IO (Maybe ClientSession)
handleAuthResponse username (LoginSuccessMessage sessionId) = return $ Just $ ClientSession sessionId username
handleAuthResponse _  msg = do 
    print $ "Server sent message: " ++ (show msg )
    return Nothing

handleAuthMessage :: [String] -> IO (Maybe ClientSession)
handleAuthMessage ["create", u, p] = handleAuthResponse (T.pack u) =<< sendMessage ( CreateUserMessage (T.pack u) (T.pack p) )
handleAuthMessage ["login", u, p] = handleAuthResponse (T.pack u) =<< sendMessage ( LoginMessage (T.pack u) (T.pack p) )
handleAuthMessage _ = return $ Nothing

handleInputL :: ClientSession -> [[String]] -> IO ()
handleInputL session messages = undefined

handleInput :: [[String]] -> IO ()
handleInput (msg:msgs) = do
    maybeSession <- handleAuthMessage msg
    case maybeSession of 
        Just session -> handleInputL session msgs
        Nothing -> handleInput msgs
handleInput [] = return () 

main :: IO ()
main = do 
    stdIn <- getContents
    let msgs = map (words) (lines stdIn)
    handleInput msgs
    where errHandler e
            | isEOFError e = return()
            | otherwise  = putStrLn (show e)

