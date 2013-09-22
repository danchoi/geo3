{-# LANGUAGE OverloadedStrings #-}
module Core where
import Data.Attoparsec.Text
import Control.Applicative
import Data.Text (Text)
import Data.Char
import qualified Data.Text as T

type LatLng = (Double, Double)
type Name = Text

data Event = Rename Name Name
           | Locate Name LatLng
           | Chat Name Text
           | Connect Name LatLng
           | Disconnect Name
           deriving (Show, Eq)

eventParser :: Parser Event
eventParser = parseRename <|> parseLocate <|> parseChat

parseName = takeWhile1 isAlphaNum

-- dan changes to tom
parseRename = do 
  n <- parseName
  string " changes to "
  n' <- parseName
  return $ Rename n n' 

-- dan moves to 42.1231232 -71.1231231
parseLocate = 
  Locate <$> (parseName <* string " moves to ") <*> ((,) <$> double <*> (char ' ' *> double))

-- dan says hello cambridge!
parseChat = Chat <$> (parseName <* string " says ") <*> takeText

-- for development
test :: String -> Either String Event
test s = parseOnly eventParser (T.pack s)
