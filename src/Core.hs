{-# LANGUAGE OverloadedStrings #-}
module Core where
import Data.Attoparsec.Text
import Control.Applicative
import Data.Text (Text)
import Data.Char
import qualified Data.Text as T

import Data.Aeson


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

eventParser :: Parser Event
eventParser = parseRename <|> parseLocate <|> parseChat

name = takeWhile1 isAlphaNum

latLng = ((,,) <$> double <*> (char ' ' *> double)) <*> (char ' ' *> decimal)

parseRename = Rename <$> name <* string " rename to " <*> name

parseLocate = Locate <$> (name <* string " loc ") <*> latLng

parseChat = Chat <$> (name <* string " chat ") <*> latLng <*> (char ' ' *> takeText)

{- test function for development -}
test :: String -> Either String Event
test s = parseOnly eventParser (T.pack s)



{- 

Examples

ghci> test "dan chat 42.123 -71.1233 12 hello cambridge!"
Right (Chat "dan" (42.123,-71.1233,12) "hello cambridge!")
ghci> test "dan rename to tom"
Right (Rename "dan" "tom")
ghci> test "dan loc 42.1231232 -71.1231231 12"
Right (Locate "dan" (42.1231232,-71.1231231,12))
ghci> 


-}
