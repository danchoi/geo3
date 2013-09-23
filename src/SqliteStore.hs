{-# LANGUAGE OverloadedStrings #-}
module SqliteStore where
import Core 
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Text (append, pack)
import Control.Concurrent 


-- note: event log will be parsed back into data types by attoparsec
textLoc lat lng zoom = 
  (pack.show $ lat) `append` " " `append`
  (pack.show $ lng) `append` " " `append`
  (pack.show $ zoom)


instance ChatStore Connection where
  getCurrentState = undefined
  getStateDiff = undefined

  insertEvent conn (Locate n (lat,lng,zoom)) = do
      quickQuery' conn 
        "insert or replace into users (user_name, user_lat, user_lng, user_zoom) values \
        \ (?, ?, ?, ?)"
        [toSql n, toSql lat, toSql lng, toSql zoom]
      quickQuery' conn
        "insert into events (event_user, event_log) values (?,?)"
        [toSql n, toSql logString]
      commit conn
      return ()
    where logString = n `append` " loc " `append` (textLoc lat lng zoom)
           

  insertEvent conn _ = undefined

  generateName conn lock = undefined

test = do
  c <- connectSqlite3 "db/test.db"
  ts <- (getTables c )
  mapM_ (putStrLn . show) ts
  insertEvent c (Locate "dan" (42.2,-71.2,13))


{-

create table events (
  event_user text,
  event_log text,
  event_created_at datetime default current_timestamp
);

create table users (
  user_name text UNIQUE,
  user_lat real,
  user_lng real,
  user_zoom num
);

create table posts (
  user_name text,
  post_lat real,
  post_lng real,
  post_zoom num,
  post_created_t datetime default current_timestamp
);

-}


{-

[choi@sparta geo3]$ sqlite3 db/test.db "select * from events"
dan|dan loc 42.2 -71.2 13|2013-09-23 18:44:07
[choi@sparta geo3]$ sqlite3 db/test.db "select * from users"
dan|42.2|-71.2|13

-}
