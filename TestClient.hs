{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}

import Network (withSocketsDo, connectTo, PortID(..), Socket)
import System.IO (hSetBuffering, hPutStrLn, hClose, BufferMode(..), Handle)
import System.IO.Error (isEOFError)

import Control.Monad
import Control.Concurrent
import qualified Control.Exception as Except

import qualified Data.Text as T
import System.Posix

import Message
import Configuration

main :: IO ()
main = withSocketsDo $ do
    _ <- installHandler sigPIPE Ignore Nothing
    conf <- Configuration.getConfiguration
    handle <- connectTo "lobby-lanarts.rhcloud.com" (PortNumber $ 80)
    loginToServer handle `Except.catch` errHandler `Except.finally` hClose handle
    where errHandler e 
            | isEOFError e = return()
            | otherwise  = putStrLn (show e) 

toMessage :: [String] -> Maybe Message
toMessage ["create", u, p] = Just $ CreateUserMessage (T.pack u) (T.pack p)
toMessage ["login", u, p] = Just $ LoginMessage (T.pack u) (T.pack p)
toMessage ("send":u:rest) = Just $ ChatMessage (T.pack u) (T.pack $ unwords rest)
toMessage _ = Nothing

loginToServer :: Handle -> IO ()
loginToServer handle = do 
    stdIn <- getContents
    let msgs = map (toMessage . words) (lines stdIn)
    _thread <- forkIO $ void $ ( sequence $ map sendMsg msgs)
    forever $ do { msg <- recvMessage handle ; print msg }
  where sendMsg (Just msg) = sendMessage handle msg
        sendMsg Nothing = putStrLn "Invalidly formatted message!"
