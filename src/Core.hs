{-# LANGUAGE OverloadedStrings #-}
module Core where
import Control.Applicative
import Data.Attoparsec.Text
import Data.Text (Text)
import Data.Char
import Data.List (partition)
import Data.Either
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson
import Data.Time.LocalTime
import Database.HDBC
import Data.Time

type LatLng = (Double, Double, Int) -- lat, lng, zoom 
type Name = Text -- alphaNumeric strings only 
type EventWithTime = (ZonedTime, Event) 
data Event = Rename Name Name
           | Locate Name LatLng
           | Chat Name LatLng Text
           | Disconnect Name
           deriving (Show, Eq, Read)

type UserHash = Text
data AuthorizeEvent = AuthorizeEvent UserHash Event
data ClientError = ClientError Text

data User = User Name LatLng
data Post = Post User Text ZonedTime
data CurrentState = CurrentState [User] [Post] ZonedTime
data StateDiff = StateDiff [Event] ZonedTime

class ChatStore a where
  getCurrentState :: IConnection a => a -> IO CurrentState
  getStateDiff :: IConnection a => a -> UTCTime -> IO StateDiff
  insertEvent :: IConnection a => a -> Event -> IO ()

{- Logic -}

-- Starting simple, we just transform the data type into JSON to broadcast

instance ToJSON Event where
  toJSON (Rename n n') = object ["from" .= n, "name" .= n']
  toJSON (Locate n l) = object ["name" .= n, "loc" .= l]
  toJSON (Chat n l t) = object ["name" .= n, "loc" .= l, "text" .= t]
  toJSON (Disconnect n) = object ["disconnect" .= n]

instance ToJSON ClientError where
  toJSON (ClientError t) = object ["error" .= t]


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

{- Dev -}
-- testJSON s = case (testParse s) of
--   Left x -> putStrLn x
--   Right x -> B.putStrLn $ encode x
-- 
{- Examples:

ghci> testJSON "dan rename to tom"
{"name":"tom","from":"dan"}

ghci> testJSON "dan chat 42.123 -71.1233 12 hello cambridge!"
{"name":"dan","text":"hello cambridge!","loc":[42.123,-71.1233,12]}

ghci> testJSON "dan loc 42.1231232 -71.1231231 12"
{"name":"dan","loc":[42.1231232,-71.1231231,12]}

-}



{- Client interaction parser -}

runParser = parseOnly clientMessage 

clientMessage :: Parser Event
clientMessage = rename <|> locate <|> chat
name = takeWhile1 isAlphaNum
latLng = ((,,) <$> double <*> (char ' ' *> double)) <*> (char ' ' *> decimal)
rename = Rename <$> name <* string " rename to " <*> name
locate = Locate <$> (name <* string " loc ") <*> latLng
chat = Chat <$> (name <* string " chat ") <*> latLng <*> (char ' ' *> takeText)

{- 

  Examples

  ghci> test "dan chat 42.123 -71.1233 12 hello cambridge!"
  Right (Chat "dan" (42.123,-71.1233,12) "hello cambridge!")
  ghci> test "dan rename to tom"
  Right (Rename "dan" "tom")
  ghci> test "dan loc 42.1231232 -71.1231231 12"
  Right (Locate "dan" (42.1231232,-71.1231231,12))

-}


{- parser test function for development -}
testParse :: String -> Either String Event
testParse s = parseOnly clientMessage (T.pack s)



{- References

  Data.Time.LocalTime 
    http://www.haskell.org/ghc/docs/6.12.2/html/libraries/time-1.1.4/Data-Time-LocalTime.html#t%3ALocalTime

  Data.Time.Clock
    http://www.haskell.org/ghc/docs/6.12.2/html/libraries/time-1.1.4/Data-Time-Clock.html#t%3AUTCTime

  "2013-09-22T16:53:22.697-0400" JSON generated by ToJSON of ZonedTime

  "2013-09-22 17:04:58 EDT" : this is readable by ZonedTime; more human readable, even if we stripped the partial seconds


read "(Locate \"dan\" (42.1231232,-71.1231231,12))"
read "(\"2013-09-22 17:04:58 EDT\", (Locate \"dan\" (42.1231232,-71.1231231,12)))" :: (ZonedTime, Event)
((read "2013-09-22 17:04:58 EDT" :: ZonedTime), (Locate (T.pack "dan") (42.1231232,-71.1231231,12)))

["2013-09-22T17:04:58-0400",{"name":"dan","loc":[42.1231232,-71.1231231,12],"t":"loc"}]

on js side:
new Date(Date.parse("2013-09-22T17:29:17.956-0400"))
new Date(Date.parse("2013-09-22T17:29:17-0400"))

onmessage:["2013-09-22T17:32:26.726-0400",{"name":"anon","text":"test","loc":[123.0,123.0,123]}]

-}
