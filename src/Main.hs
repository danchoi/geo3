{-# LANGUAGE OverloadedStrings, ScopedTypeVariables  #-}

module Main where

import Control.Monad (when, forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Control.Concurrent
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

import Core

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

removeClientSink :: ClientSink -> MVar ServerState -> IO ()
removeClientSink c state = do
    modifyMVar_ state $ \s -> do
      let s' = filter (/= c) s
      return s'
    return ()

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn message
    forM_ clients $ \sink -> W.sendSink sink $ W.textData message

websocket :: MVar ServerState -> W.Request -> W.WebSockets W.Hybi10 ()
websocket state rq = do
    W.acceptRequest rq
    W.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
    W.spawnPingThread 30 :: W.WebSockets W.Hybi10 ()
    sink <- W.getSink
    liftIO $ putStrLn $ "Creating client " 
    -- TODO gen anon default name and store value in closure in call to receiveMessage
    liftIO $ modifyMVar_ state $ \s -> do
        let s' = addClientSink sink s
        W.sendSink sink $ W.textData $ T.pack "hello handshake"
        return s'
    receiveMessage "anon" state sink

receiveMessage :: Text -> MVar ServerState -> ClientSink -> W.WebSockets W.Hybi10 ()
receiveMessage name state sink = flip W.catchWsError catchDisconnect $ do
    m <- W.receiveData 
    -- parse m here
    liftIO $ readMVar state >>= broadcast m 
    liftIO (putStrLn $ "received data: " ++ (T.unpack m))
    receiveMessage name state sink
  where
    catchDisconnect e = case fromException e of
        Just W.ConnectionClosed -> do 
            liftIO $ putStrLn  "connection closed"
            liftIO $ removeClientSink sink state
            return ()
        _ -> do 
            liftIO $ putStrLn "Uncaught Error"
            return ()

main :: IO ()
main = do
  serverState <- newMVar []
  forkIO $ forever $ do
    eof <- isEOF
    when (not eof) $ do
      line <- getLine 
      putStrLn $ "Gotline " ++ line
      readMVar serverState >>= broadcast (T.pack line)
  putStrLn "starting server"
  httpServe simpleConfig $ site serverState 

site :: MVar ServerState -> Snap ()
site s = ifTop (serveFile "public/index.html") <|> 
    route [ ("ws", runWebSocketsSnap $ websocket s) ] <|>
    route [ ("", (serveDirectory "public")) ] 


{-

http://hackage.haskell.org/packages/archive/websockets/0.6.0.3/doc/html/Network-WebSockets.html

In some cases, you want to escape from the WebSockets monad and send data to the websocket from different threads. To this end, the getSink method is provided. The next example spawns a thread which continuously spams the client in another thread:

 import Control.Concurrent (forkIO)
 import Control.Monad (forever)
 import Control.Monad.Trans (liftIO)
 import Network.WebSockets
 import qualified Data.Text as T
 
 spam :: TextProtocol p => WebSockets p ()
 spam = do
     sink <- getSink
     _ <- liftIO $ forkIO $ forever $
         sendSink sink $ textData (T.pack "SPAM SPAM SPAM!")
     sendTextData (T.pack "Hello world!")

-}
