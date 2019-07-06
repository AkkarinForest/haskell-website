{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Monad.Logger (runStdoutLoggingT, MonadLogger, LoggingT)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Int (Int64)
import           Database.Persist (get, insert, delete, selectList)
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import           Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, runMigration, SqlPersistT)
import Database.Persist.Types (PersistFilter (Lt), Entity)

import           Schema

localConnString :: ConnectionString
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=postgres"

fetchPostgresConnection :: IO ConnectionString
fetchPostgresConnection = return localConnString

fetchSymbolPG :: ConnectionString -> Int64 -> IO (Maybe Symbol)
fetchSymbolPG connString uid = runAction connString (get (toSqlKey uid))

fetchSymbolsStatsPG :: ConnectionString -> IO [Entity Symbol]
fetchSymbolsStatsPG connString = runAction connString (selectList [] [])

createSymbolPG :: ConnectionString -> Symbol -> IO Int64
createSymbolPG connString symbol = fromSqlKey <$> runAction connString (insert symbol)

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action =
  runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend

migrateDB :: ConnectionString -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)
