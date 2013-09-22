{-# LANGUAGE OverloadedStrings #-}
module Core where
import Data.Attoparsec.Text
import Control.Applicative
import Data.Text (Text)
import Data.Char
import qualified Data.Text as T

type LatLng = (Double, Double)

data Event = Rename Text Text
           | Locate Text LatLng
           | Chat Text Text
           deriving (Show, Eq)

eventParser :: Parser Event
eventParser = parseRename <|> parseLocate <|> parseChat

parseName = takeWhile1 isAlphaNum

-- rename dan tom
parseRename = Rename <$> (string "rename" >> char ' ' *> parseName) <*> (char ' ' *> parseName) 

-- locate dan 42.1231232 -71.123
parseLocate = do
  string "locate" 
  Locate <$> (char ' ' *> parseName) <*> ((,) <$> (char ' ' *> double) <*> (char ' ' *> double))

-- chat dan hello cambridge!
parseChat = Chat <$> (string "chat" >> char ' ' *> parseName <* char ' ') <*> takeTill (== '\n') 

-- for development
test :: String -> Either String Event
test s = parseOnly eventParser (T.pack s)
