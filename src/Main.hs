{-# LANGUAGE OverloadedStrings, ScopedTypeVariables  #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Control.Concurrent (MVar, newMVar, readMVar, modifyMVar_)
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

import qualified Network.WebSockets as WS

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


type ClientSink = WS.Sink WS.Hybi10
type ServerState = [ClientSink]

addClientSink :: ClientSink -> ServerState -> ServerState
addClientSink c s = c:s 

removeClientSink :: ClientSink -> ServerState -> ServerState
removeClientSink = undefined


wsApplication :: MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi10 ()
wsApplication state rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
    WS.spawnPingThread 30 :: WS.WebSockets WS.Hybi10 ()
    sink <- WS.getSink
    sinks <- liftIO $ readMVar state
    liftIO $ putStrLn $ "Create client " 
    liftIO $ modifyMVar_ state $ \s -> do
        let s' = addClientSink sink s
        WS.sendSink sink $ WS.textData $ T.pack "hello handshake"
        return s'
    receiveMessage state sink

receiveMessage :: WS.Protocol p => MVar ServerState -> ClientSink -> WS.WebSockets p ()
receiveMessage state sink = flip WS.catchWsError catchDisconnect $ do
    rawMsg <- WS.receiveData 
    liftIO (putStrLn $ "receiveData: " ++ (T.unpack rawMsg))
    receiveMessage state sink
  where
    catchDisconnect e = case fromException e of
        Just WS.ConnectionClosed -> do 
            liftIO $ putStrLn  "connection closed"
            return ()
        _ -> do 
            liftIO $ putStrLn "Uncaught Error"
            return ()




main :: IO ()
main = httpServe simpleConfig $ site 

site :: Snap ()
site = ifTop (serveFile "public/index.html") <|> 
    route [ ("ws", liftIO (newMVar []) >>= runWebSocketsSnap . wsApplication) ] <|>
    route [ ("", (serveDirectory "public")) ] 

