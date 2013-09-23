{-# LANGUAGE OverloadedStrings #-}
module SqliteStore where
import Core 
import Database.HDBC.Sqlite3


instance ChatStore Connection where
  getCurrentState = undefined
  getStateDiff = undefined



