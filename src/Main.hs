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
import Network.WebSockets.Snap 
import qualified Network.WebSockets as W
import System.IO (isEOF, stdin)

import qualified Data.Map as M
import Data.Aeson (encode, ToJSON)

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

type ClientSink = (Name, W.Sink W.Hybi10)
type ServerState = M.Map Name ClientSink

addClientSink :: ClientSink -> ServerState -> ServerState
addClientSink (name,sink) s = M.insert name (name,sink) s

removeClientSink :: ClientSink -> MVar ServerState -> IO ()
removeClientSink (name,_) state = do
    modifyMVar_ state $ \s -> do
      let s' = M.delete name s
      return s'
    return ()

debugClients :: MVar ServerState -> IO ()
debugClients st = liftIO $ do
  readMVar st >>= putStrLn . show . map fst . M.elems

broadcast :: Text -> MVar ServerState -> IO ()
broadcast message s = do
  T.putStrLn message
  currentState <- readMVar s
  forM_ (nubBy myNubFn . M.elems $ currentState) $ \c@(name, sink) -> do 
    Control.Exception.catch 
      (W.sendSink sink $ W.textData message) 
      (\e -> do 
        let err = show (e :: IOException)
        putStrLn ("Caught exception in broadcast while sending to "++T.unpack name++": " ++ err)
        removeClientSink c s
        return ()
        )
  where myNubFn c1 c2 = snd c1 == snd c2  -- unique sinks

makeName :: M.Map Text a -> Text -> Text
makeName m k = makeName2 k 
  where 
    makeName2 x = 
      case (M.lookup x m) of
        Nothing -> x
        Just _ -> makeName2 (incName x) 

websocket :: MVar ServerState -> W.Request -> W.WebSockets W.Hybi10 ()
websocket state rq = do
    W.acceptRequest rq
    W.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
    sink <- W.getSink
    name <- liftIO $ modifyMVar state $ \s -> do
        -- see if the sink already exists; else add
        let existingSink = filter (\(name, sink') -> sink' == sink) (M.elems s)
        -- putStrLn $ show $ length existingSink
        case existingSink of
          ((name', sink'):_) ->
            return (s, name')
          otherwise -> do
            let name = makeName s "anon"
            let s' = addClientSink (name,sink) s
            return (s', name)
    liftIO $ putStrLn $ "Creating client " ++ (T.unpack name)
    W.send . W.textData $ name
    receiveMessage state (name,sink)

receiveMessage :: MVar ServerState -> ClientSink -> W.WebSockets W.Hybi10 ()
receiveMessage state c@(name,sink) = flip W.catchWsError catchDisconnect $ do
    dm <- W.receive
    case dm of 
      (W.ControlMessage x@(W.Close _)) -> do
        liftIO $ putStrLn $ "received control mesage " ++ (show x)
        liftIO $ removeClientSink c state
      (W.DataMessage (W.Text x)) -> do
        let m = TE.decodeUtf8 . B.concat . BL.toChunks $ x
        liftIO (putStrLn $ "received data: " ++ (T.unpack m))
        -- prepend name to message before sending it to parser (and logger) TODO
        case (parseMessage $ name `T.append` " " `T.append` m) of 
          Right (Rename n n') -> do 
            newName <- liftIO $ modifyMVar state $ \s -> do
              let n'' = makeName s n'
              case (M.lookup n s) of
                Nothing -> return (s, n'') -- shouldn't happen!
                Just (_, sink) -> do 
                  let s' = M.delete n s
                  let s'' = M.insert n'' (n'',sink) s'
                  return (s'', n'')
            process (Rename n newName) state 
            receiveMessage state (newName, sink)
          Right m' -> do 
            process m' state 
            receiveMessage state c
          Left x -> do 
            liftIO . T.putStrLn $ "Could not parse message: " `T.append` m
            W.send (W.textData . encodeToText $ ClientError "Could not parse message")
            receiveMessage state c

  where
    catchDisconnect e = case fromException e of
      Just W.ConnectionClosed -> do 
          liftIO $ T.putStrLn $ "connection closed by " `T.append` name
          liftIO $ removeClientSink c state
          t <- liftIO getZonedTime
          liftIO $ broadcast (encodeToText (t, (Disconnect name))) state
      _ -> do 
          liftIO $ putStrLn "Uncaught Error"

{- helper to encode to JSON as Text -}
encodeToText :: ToJSON a => a -> Text
encodeToText = TE.decodeUtf8.B.concat.BL.toChunks.encode 

{- Core processing -}
process :: Event -> MVar ServerState -> W.WebSockets W.Hybi10 ()

process x s = liftIO $ do
    t <- getZonedTime 
    broadcast (encodeToText x) s


main :: IO ()
main = do
  serverState <- newMVar M.empty
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
