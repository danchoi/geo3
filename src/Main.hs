{-# LANGUAGE OverloadedStrings, ScopedTypeVariables  #-}

module Main where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception (fromException)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import Data.List (foldl')

import Snap.Core
import Snap.Http.Server.Config
import Snap.Http.Server 
import Snap.Util.FileServe

import Network.WebSockets.Snap 
import qualified Network.WebSockets as W
import System.IO (isEOF, stdin)

simpleConfig :: Config m a
simpleConfig = foldl' (\accum new -> new accum) emptyConfig base where
    base = [hostName, accessLog, errorLog, locale, port, ip, verbose]
    hostName = setHostname (bsFromString "localhost")
    accessLog = setAccessLog (ConfigFileLog "access.log")
    errorLog = setErrorLog (ConfigFileLog "error.log")
    locale = setLocale "US"
    port = setPort 9160
    ip = setBind (bsFromString "127.0.0.1")
    verbose = setVerbose True
    bsFromString = TE.encodeUtf8 . T.pack


type ClientSink = W.Sink W.Hybi10
type ServerState = [ClientSink]

addClientSink :: ClientSink -> ServerState -> ServerState
addClientSink c s = c:s 

removeClientSink :: ClientSink -> ServerState -> ServerState
removeClientSink = undefined

wsApplication :: MVar ServerState -> Chan String -> W.Request -> W.WebSockets W.Hybi10 ()
wsApplication state ch rq = do
    W.acceptRequest rq
    W.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
    W.spawnPingThread 30 :: W.WebSockets W.Hybi10 ()
    sink <- W.getSink
    liftIO $ putStrLn $ "Creating client " 
    liftIO $ modifyMVar_ state $ \s -> do
        let s' = addClientSink sink s
        W.sendSink sink $ W.textData $ T.pack "hello handshake"
        return s'
    liftIO (forkIO (readChan ch >>= putStrLn))
    receiveMessage state sink

receiveMessage ::  MVar ServerState -> ClientSink -> W.WebSockets W.Hybi10 ()
receiveMessage state sink = flip W.catchWsError catchDisconnect $ do
    rawMsg <- W.receiveData 
    liftIO (putStrLn $ "receiveData: " ++ (T.unpack rawMsg))
    liftIO $ putStrLn "Getline"
    -- read input from channel
    z <- liftIO (isEOF)
    if (not z) 
      then do
        x <- liftIO getLine
        liftIO $ putStrLn $ "Gotline: " ++ x
        W.send $ W.textData $ T.pack $ "Gotline: " ++ x
      else  return ()

    receiveMessage state sink
  where
    catchDisconnect e = case fromException e of
        Just W.ConnectionClosed -> do 
            liftIO $ putStrLn  "connection closed"
            return ()
        _ -> do 
            liftIO $ putStrLn "Uncaught Error"
            return ()

readInput ch = do
  eof <- isEOF
  when (not eof) $ do
    x <- getLine
    putStrLn $ "Gotline: " ++ x ++ " Writing to chan"
    writeChan ch x
  readInput ch

main :: IO ()
main = do
  serverState <- newMVar []
  ch <- newChan
  forkIO $ readInput ch
  httpServe simpleConfig $ site serverState ch

site :: MVar ServerState -> Chan String -> Snap ()
site s ch = ifTop (serveFile "public/index.html") <|> 
    route [ ("ws", (runWebSocketsSnap $ wsApplication s ch)) ] <|>
    route [ ("", (serveDirectory "public")) ] 

