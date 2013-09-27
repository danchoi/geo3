{-# LANGUAGE OverloadedStrings #-}
module Core where
import Control.Applicative
import Data.Attoparsec.Text hiding (Result)
import Data.Text (Text, pack, append)
import Data.Char
import Data.List (partition)
import Data.Either
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Time.LocalTime
import Database.HDBC
import Data.Time
import Data.UUID.V4
import Data.UUID (toString, UUID)
import System.Time
import Data.Aeson hiding (Result, Success)

data LatLng = LatLng {
    getLat :: Double, getLng :: Double, getZoom :: Int 
  } deriving (Show, Read, Eq)

type Session = Int 
type Name = Text -- alphaNumeric strings only 
data Event = NewSession Text  -- text is nickname
           | Rename Session Name
           | Move Session LatLng
           | Chat Session Text
           | Disconnect Session
           deriving (Show, Eq, Read)
data Result = Success | NewSessionInfo Int String
type Sha1 = Text
data ClientError = ClientError Text

data Post = Post Session Text ZonedTime

instance ToJSON Result where
  toJSON Success = object ["success" .= ("ok" :: Text)]
  toJSON (NewSessionInfo s uuid) = object ["session" .= s, "uuid" .= uuid]

{- Client interaction parser -}

runParser = parseOnly clientMessage 
runAuthorizedParser = parseOnly authorizedClientMessage 

authorizedClientMessage :: Parser (Text, Event)
authorizedClientMessage = (,) <$> takeWhile1 (not.isSeparator) <*> (char ' ' *> clientMessage)

clientMessage :: Parser Event
clientMessage = newSession <|> rename <|> move <|> chat
name = takeWhile1 isAlphaNum
latLng = LatLng <$> double <*> (char ' ' *> double) <*> (char ' ' *> decimal)
newSession = NewSession <$> (string "connect " *> name)
rename = Rename <$> decimal <* string " rename to " <*> name
move = Move <$> (decimal <* string " move to ") <*> latLng
chat = Chat <$> (decimal <* string " chat ") <*> takeText

{- 

  "connect dan"  => NewSession "dan"
  "12 chat hello cambridge!" => Chat 12 "hello cambridge!"
  "12 rename to tom" => Rename 12 "tom"
  "12 move to 42.1231232 -71.1231231 12" => Move 12 (LatLng 42.1231232 -71.1231231 12)

-}


{- parser test function for development -}
testParse :: String -> Either String Event
testParse s = parseOnly clientMessage (T.pack s)


-- Storage

processEvent :: IConnection a => a -> Event -> IO Result

processEvent conn (NewSession name) = do
    hash <- (nextRandom :: IO UUID)
    _ <- quickQuery' conn 
      "insert into sessions (session_nickname, session_opaque_uuid) values (?,?)"
      [toSql name, toSql . toString $ hash]
    [[s]] <- quickQuery' conn "select last_insert_rowid()" []
    [[uuid]] <- quickQuery' conn "select session_opaque_uuid from sessions where session = ?" [s]
    commit conn
    return $ NewSessionInfo (fromSql s :: Int) (fromSql uuid :: String)


processEvent conn (Rename s n) = do
  -- TODO update last_updated_at
  run conn "update sessions set session_nickname = ? where session = ?" [toSql n, toSql s] 
  commit conn 
  return Success

processEvent conn (Move s (LatLng lat lng zoom)) = do
  run conn 
    "update sessions set session_lat = ?, session_lng = ?, session_zoom = ? \
    \where session = ?" 
    [toSql lat, toSql lng, toSql zoom, toSql s]
  commit conn
  return Success

processEvent conn (Chat s t) = do
  [[nick,lat,lng,zoom]] <- quickQuery' conn
    "select session_nickname, session_lat, session_lng, session_zoom from \
    \sessions where session = ?" [toSql s]
  run conn 
    "insert into posts(session, post_author, post_text, post_lat, post_lng, post_zoom) \
    \values (?,?,?,?,?,?)" [toSql s, nick, toSql t, lat, lng, zoom]
  commit conn
  return Success

processEvent conn (Disconnect s) = do
  t <- getClockTime
  run conn "update sessions set session_disconnected_at = datetime(?, 'unixepoch') \
  \where session = ?" 
    [toSql t, toSql s]
  commit conn
  return Success

-- used to generate unique names; increments number at end of name
incName :: Text -> Text
incName x = 
    let s = T.unpack x
        s' = reverse s
        (ds, remainder) = partition isDigit s'
        ds' = if null ds 
              then "1"
              else (show $ (((read.reverse $ ds) :: Int) + 1))
        str = (reverse remainder) ++ ds'
     in T.pack str




