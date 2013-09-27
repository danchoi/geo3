{-# LANGUAGE OverloadedStrings, ScopedTypeVariables  #-}

module Main where

import Control.Monad (when, forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Control.Concurrent
import Control.Exception 
import System.IO (stderr, hPutStrLn)
import Data.Time.LocalTime
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import Data.List (foldl', nubBy)
import Snap.Core
import Snap.Http.Server.Config
import Snap.Http.Server 
import Snap.Util.FileServe
import Data.Maybe (fromJust)
import System.IO (isEOF, stdin)

import qualified Data.Map as M
import Data.Aeson (encode, ToJSON)
import Database.HDBC
import Database.HDBC.Sqlite3
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


startWeb :: IO ()
startWeb = do
  serverState <- newMVar []
  putStrLn "starting server"
  httpServe simpleConfig $ site 

site :: Snap ()
site = ifTop (serveFile "public/index.html") <|> 
    route [ ("", (serveDirectory "public")) ] 

test  = do 
    c <- connectSqlite3 "db/test.db"
    (s, uuid) <- createSession c "Hello"
    putStrLn $ show (s,uuid)
    processEvent c (Move s (LatLng 42 71 13))
    processEvent c (Chat s "Test chatting!")
    processEvent c (Disconnect s)
    disconnect c

main = startWeb



{- helper to encode to JSON as Text -}

encodeToText :: ToJSON a => a -> Text
encodeToText = TE.decodeUtf8.B.concat.BL.toChunks.encode 


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
