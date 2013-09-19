{-# LANGUAGE OverloadedStrings #-}

module WebsocketServer 
  (wsApplication) where

import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad (forM_, liftM)
import qualified Data.Map as M
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.OldException
import Control.Exception (fromException)
import qualified Network.WebSockets as WS
import Data.Text.Lazy.Encoding as E
import Data.Aeson 
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import Control.Monad.IO.Class (liftIO)

type ClientSink = WS.Sink WS.Hybi10

type ServerState = [ClientSink]

addClientSink :: ClientSink -> ServerState -> ServerState
addClientSink c s = c:s 

removeClientSink :: ClientSink -> ServerState -> ServerState
removeClientSink = undefined

singlecast :: Text -> ClientSink -> IO ()
singlecast m c = 
    WS.sendSink c $ WS.textData $ encode m

wsApplication :: MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi10 ()
wsApplication state rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
    WS.spawnPingThread 30  :: WS.WebSockets WS.Hybi10 ()
    sink <- WS.getSink
    sinks <- liftIO $ readMVar state
    liftIO $ putStrLn $ "Create client " `mappend` "test"
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


    {-
    case (decode rawMsg :: Maybe MessageFromClient) of
        Just (MapBoundsUpdated sw ne) -> do 
            liftIO $ modifyMVar_ state $ \s -> do
                let s' = updateClientSinkBounds client sw ne s
                return s'
        Just m@(ListActiveRooms sw ne) -> do 
            msgsFromServer <- liftIO $ processMsg conn client m
            liftIO $ singlecast msgsFromServer sink
        Just clientMessage -> do 
            msgsFromServer <- liftIO $ processMsg conn client clientMessage
            liftIO $ putStrLn $ "about to broadcast: " ++ (show msgsFromServer)
            liftIO $ broadcast msgsFromServer state
            return ()
        Nothing -> do 
            let errMsg = (E.decodeUtf8 rawMsg)
            liftIO $ TL.putStrLn $ "Failed to decode: " `mappend`  errMsg
            return () 
    -}



