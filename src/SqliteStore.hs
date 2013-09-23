{-# LANGUAGE OverloadedStrings #-}
module SqliteStore where
import Core 
import Database.HDBC
import Database.HDBC.Sqlite3

instance ChatStore Connection where
  getCurrentState = undefined
  getStateDiff = undefined

  insertEvent conn (Locate n (lat,lng,zoom)) = do
    quickQuery' conn 
      "insert or replace into users (user_name, user_lat, user_lng, user_zoom) values \
      \ (?, ?, ?, ?)"
      [toSql n, toSql lat, toSql lng, toSql zoom]
    commit conn
    return ()

  insertEvent conn _ = undefined


test = do
  c <- connectSqlite3 "db/test.db"
  ts <- (getTables c )
  mapM_ (putStrLn . show) ts
  insertEvent c (Locate "dan" (42.2,-71.2,13))


{-

create table events (
  event_user text,
  event_log text,
  event_created_at datetime
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
  post_created_t datetime
);

-}

