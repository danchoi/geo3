module Main where
import Core
import Database.HDBC
import Database.HDBC.Sqlite3
import SqliteStore


connect = connectSqlite3 "db/test.db"

main = do
  c <- connect
  fmap print $ getTables c 
  putStrLn "hello"
