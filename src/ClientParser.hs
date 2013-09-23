{-# LANGUAGE OverloadedStrings #-}
module ClientParser where
import Data.Attoparsec.Text
import Core

{- Parsers -}

parseMessage = parseOnly clientMessage 

clientMessage :: Parser Event
clientMessage = parseRename <|> parseLocate <|> parseChat
name = takeWhile1 isAlphaNum
latLng = ((,,) <$> double <*> (char ' ' *> double)) <*> (char ' ' *> decimal)
parseRename = Rename <$> name <* string " rename to " <*> name
parseLocate = Locate <$> (name <* string " loc ") <*> latLng
parseChat = Chat <$> (name <* string " chat ") <*> latLng <*> (char ' ' *> takeText)

{- Examples

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

main = do 
  putStrLn "hello"
