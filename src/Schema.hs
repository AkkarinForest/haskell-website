{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Schema where

import           Data.Aeson (ToJSON, toJSON, object, (.=), FromJSON, parseJSON, (.:), withObject
                            , Object)
import           Data.Aeson.Types (Parser, Pair)
import           Database.Persist (Entity(..), Entity)
import qualified Database.Persist.TH as PTH
import           Data.Text (Text)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  Symbol sql=symbols
    symbol Text

  Stroke sql=strokes
    symbolId SymbolId
    delayTime Int
|]

instance ToJSON Symbol where 
  toJSON symbol = object 
    [ "symbol" .= symbolSymbol symbol ]

instance FromJSON Symbol where
  parseJSON = withObject "Symbol" parserSymbol

parserSymbol :: Object -> Parser Symbol
parserSymbol o = do
  sSymbol <- o .: "symbol"
  return Symbol
    { symbolSymbol = sSymbol }
