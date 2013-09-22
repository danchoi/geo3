{-# LANGUAGE OverloadedStrings #-}
module Core where
import Data.Attoparsec.Text
import Control.Applicative
import Data.Text (Text)
import Data.Char
import Data.Either
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson


{- Types -}

{- lat, lng, zoom -}
type LatLng = (Double, Double, Int)

{- alphaNumeric strings only -}
type Name = Text

data Event = Rename Name Name
           | Locate Name LatLng
           | Chat Name LatLng Text
           | Connect Name LatLng
           | Disconnect Name
           deriving (Show, Eq)



{- Parsers -}

eventParser :: Parser Event
eventParser = parseRename <|> parseLocate <|> parseChat

name = takeWhile1 isAlphaNum

latLng = ((,,) <$> double <*> (char ' ' *> double)) <*> (char ' ' *> decimal)

parseRename = Rename <$> name <* string " rename to " <*> name

parseLocate = Locate <$> (name <* string " loc ") <*> latLng

parseChat = Chat <$> (name <* string " chat ") <*> latLng <*> (char ' ' *> takeText)

{- 

  Examples

  ghci> test "dan chat 42.123 -71.1233 12 hello cambridge!"
  Right (Chat "dan" (42.123,-71.1233,12) "hello cambridge!")
  ghci> test "dan rename to tom"
  Right (Rename "dan" "tom")
  ghci> test "dan loc 42.1231232 -71.1231231 12"
  Right (Locate "dan" (42.1231232,-71.1231231,12))

-}


{- Logic -}

-- Starting simple, we just transform the data type into JSON to broadcast

instance ToJSON Event where
  toJSON (Rename n n') = object ["t" .= ("rename" :: Text), "from" .= n, "to" .= n']
  toJSON (Locate n l) = object ["t" .= ("loc" :: Text), "name" .= n, "loc" .= l]
  toJSON (Chat n l t) = object ["t" .= ("chat" :: Text), "name" .= n, "loc" .= l, "text" .= t]
  toJSON (Connect n l) = object ["t" .= ("connect" :: Text), "name" .= n, "loc" .= l]
  toJSON (Disconnect n) = object ["t" .= ("disconnect" :: Text), "name" .= n]



{- Dev -}

{- parser test function for development -}
testParse :: String -> Either String Event
testParse s = parseOnly eventParser (T.pack s)

testJSON s = case (testParse s) of
  Left x -> putStrLn x
  Right x -> B.putStrLn $ encode x

{- Examples:

ghci> testJSON "dan rename to tom"
{"to":"tom","from":"dan","t":"rename"}
ghci> testJSON "dan chat 42.123 -71.1233 12 hello cambridge!"
{"name":"dan","text":"hello cambridge!","loc":[42.123,-71.1233,12],"t":"chat"}
ghci> testJSON "dan loc 42.1231232 -71.1231231 12"
{"name":"dan","loc":[42.1231232,-71.1231231,12],"t":"loc"}

-}

