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
import qualified Network.WebSockets as W

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


wsApplication :: MVar ServerState -> W.Request -> W.WebSockets W.Hybi10 ()
wsApplication state rq = do
    W.acceptRequest rq
    W.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
    W.spawnPingThread 30 :: W.WebSockets W.Hybi10 ()
    sink <- W.getSink
    sinks <- liftIO $ readMVar state
    liftIO $ putStrLn $ "Create client " 
    liftIO $ modifyMVar_ state $ \s -> do
        let s' = addClientSink sink s
        W.sendSink sink $ W.textData $ T.pack "hello handshake"
        return s'
    receiveMessage state sink

receiveMessage :: W.Protocol p => MVar ServerState -> ClientSink -> W.WebSockets p ()
receiveMessage state sink = flip W.catchWsError catchDisconnect $ do
    rawMsg <- W.receiveData 
    liftIO (putStrLn $ "receiveData: " ++ (T.unpack rawMsg))
    receiveMessage state sink
  where
    catchDisconnect e = case fromException e of
        Just W.ConnectionClosed -> do 
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

