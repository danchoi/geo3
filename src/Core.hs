{-# LANGUAGE OverloadedStrings #-}
module Core where
import Control.Applicative
import Data.Attoparsec.Text
import Data.Text (Text, pack, append)
import Data.Char
import Data.List (partition)
import Data.Either
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson
import Data.Time.LocalTime
import Database.HDBC
import Data.Time
import Data.UUID.V4
import Data.UUID (toString, UUID)


data LatLng = LatLng {
    getLat :: Double, getLng :: Double, getZoom :: Int 
  } deriving (Show, Read, Eq)

type Session = Int 
type Name = Text -- alphaNumeric strings only 
data Event = Rename Session Name
           | Move Session LatLng
           | Chat Session Text
           | Disconnect Session
           deriving (Show, Eq, Read)

type Sha1 = Text
data ClientError = ClientError Text

data Post = Post Session Text ZonedTime

{- Client interaction parser -}

runParser = parseOnly clientMessage 

clientMessage :: Parser Event
clientMessage = rename <|> move <|> chat
name = takeWhile1 isAlphaNum
latLng = LatLng <$> double <*> (char ' ' *> double) <*> (char ' ' *> decimal)
rename = Rename <$> decimal <* string " rename to " <*> name
move = Move <$> (decimal <* string " move to ") <*> latLng
chat = Chat <$> (decimal <* string " chat ") <*> takeText

{- 

  Examples

  ghci> testParse "12 chat 42.123 -71.1233 12 hello cambridge!"
  Right (Chat 12 (42.123,-71.1233,12) "hello cambridge!")
  ghci> testParse "12 rename to tom"
  Right (Rename 12 "tom")
  ghci> testParse "12 move to 42.1231232 -71.1231231 12"
  Right (Move 12 (42.1231232,-71.1231231,12))

-}


{- parser test function for development -}
testParse :: String -> Either String Event
testParse s = parseOnly clientMessage (T.pack s)


-- Storage

-- returns session int and session uuid 
createSession :: IConnection a => a -> Text -> IO (Int, String)
createSession conn name = do
    hash <- (nextRandom :: IO UUID)
    _ <- quickQuery' conn 
      "insert into sessions (session_nickname, session_security_hash) values (?,?)"
      [toSql name, toSql . toString $ hash]
    [[s]] <- quickQuery' conn "select last_insert_rowid()" []

    [[uuid]] <- quickQuery' conn "select session_security_hash from sessions where session = ?" [s]
    commit conn
    return (fromSql s :: Int, fromSql uuid :: String)
  

processEvent :: IConnection a => a -> Event -> IO ()

processEvent conn (Rename s n) = undefined
processEvent conn (Move s (LatLng lat lng zoom)) = do
  quickQuery' conn 
    "update sessions set session_lat = ?, session_lng = ?, session_zoom = ? \
    \where session = ?" 
    [toSql lat, toSql lng, toSql zoom, toSql s]
  commit conn
  return ()
processEvent conn (Chat s t) = undefined
processEvent conn (Disconnect s) = undefined


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




