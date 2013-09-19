{-# LANGUAGE OverloadedStrings, ScopedTypeVariables  #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Control.Concurrent (MVar, newMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE

import Snap.Core
import Snap.Http.Server.Config
import Snap.Http.Server 
import Snap.Util.FileServe

import Network.WebSockets.Snap 
import Data.List (foldl')

import WebsocketServer (wsApplication)

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

main :: IO ()
main = httpServe simpleConfig $ site 

site :: Snap ()
site = ifTop (serveFile "public/index.html") <|> 
    route [ ("ws", liftIO (newMVar []) >>= runWebSocketsSnap . wsApplication) ] <|>
    route [ ("", (serveDirectory "public")) ] 

