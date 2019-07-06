{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Database.Persist (Key, Entity)
import           Database.Persist.Postgresql (ConnectionString)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Client
import           Servant.Server
import           Servant.Server.StaticFiles (serveDirectoryFileServer)

import           Database (fetchPostgresConnection, fetchSymbolPG, fetchSymbolsStatsPG, createSymbolPG)
import           Schema

type SymbolsAPI =
  "symbols" :> Capture "symbolid" Int64 :> Get '[JSON] Symbol :<|>
  "symbols" :> ReqBody '[JSON] Symbol :> Post '[JSON] Int64 :<|>
  "symbols_stats" :> Get '[JSON] [Entity Symbol] :<|>
  Raw

symbolsAPI :: Proxy SymbolsAPI
symbolsAPI = Proxy :: Proxy SymbolsAPI

fetchSymbolsHandler :: ConnectionString -> Int64 -> Handler Symbol
fetchSymbolsHandler connString uid = do
   maybeSymbol <- liftIO $ fetchSymbolPG connString uid
   case maybeSymbol of
     Just symbol -> return symbol
     Nothing -> Handler $ (throwE $ err401 { errBody = "err" })

createSymbolsHandler :: ConnectionString -> Symbol -> Handler Int64
createSymbolsHandler connString symbol = liftIO $ createSymbolPG connString symbol

fetchAllSymbolsHandler :: ConnectionString -> Handler [Entity Symbol]
fetchAllSymbolsHandler connString = do
  symbolsStats <- liftIO $ fetchSymbolsStatsPG connString
  return symbolsStats

symbolsServer :: ConnectionString -> Server SymbolsAPI
symbolsServer connString = (fetchSymbolsHandler connString)
  :<|> (createSymbolsHandler connString)
  :<|> (fetchAllSymbolsHandler connString)
  :<|> serveDirectoryFileServer "static/"


runServer :: IO ()
runServer = do
  connString <- fetchPostgresConnection
  run 8001 (serve symbolsAPI (symbolsServer connString))
