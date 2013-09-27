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
import Control.Concurrent 

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
clientMessage = rename <|> locate <|> chat
name = takeWhile1 isAlphaNum
latLng = LatLng <$> double <*> (char ' ' *> double) <*> (char ' ' *> decimal)
rename = Rename <$> decimal  <* string " rename to " <*> name
locate = Move <$> (decimal  <* string " loc ") <*> latLng
chat = Chat <$> (decimal  <* string " chat ") <*> (char ' ' *> takeText)

{- 

  Examples

  ghci> test "dan chat 42.123 -71.1233 12 hello cambridge!"
  Right (Chat "dan" (42.123,-71.1233,12) "hello cambridge!")
  ghci> test "dan rename to tom"
  Right (Rename "dan" "tom")
  ghci> test "dan loc 42.1231232 -71.1231231 12"
  Right (Move "dan" (42.1231232,-71.1231231,12))

-}


{- parser test function for development -}
testParse :: String -> Either String Event
testParse s = parseOnly clientMessage (T.pack s)


-- Storage

createSession :: IConnection a => a -> IO Session
createSession = undefined

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




