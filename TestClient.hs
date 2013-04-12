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

quiet (Just a) = a
quiet (Nothing) = undefined

toURI = quiet . URI.parseURI

message2request msg = HTTP.Request (toURI "http://lobby-lanarts.rhcloud.com/") HTTP.POST headers str 
  where str = (BS.concat . BSL.toChunks) $ JSON.encode msg
        headers = [ HTTP.mkHeader HTTP.HdrContentLength (show (BS.length str)) ]

handleMessage :: Maybe Message -> IO () 
handleMessage (Just msg) = do
    let request = message2request msg
    result <- HTTP.simpleHTTP request
    handleResult result
  where 
    handleResult (Left err) = putStrLn $ "ERROR OCCURRED"
    handleResult (Right response) = putStrLn $ T.unpack $ T.decodeUtf8 $ HTTP.rspBody response
handleMessage Nothing = putStrLn "Invalidly formatted message!"

toMessage :: [String] -> Maybe Message
toMessage ["create", u, p] = Just $ CreateUserMessage (T.pack u) (T.pack p)
toMessage ["login", u, p] = Just $ LoginMessage (T.pack u) (T.pack p)
toMessage ("send":u:rest) = Just $ ChatMessage (T.pack u) (T.pack $ unwords rest)
toMessage _ = Nothing

main :: IO ()
main = do 
    stdIn <- getContents
    let msgs = map (toMessage . words) (lines stdIn)
    void $ sequence $ map handleMessage msgs
    where errHandler e
            | isEOFError e = return()
            | otherwise  = putStrLn (show e)

