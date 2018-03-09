{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module DatabaseStore where

import Control.DeepSeq
import Data.Text
import Data.Typeable
import GHC.Generics
import React.Flux

data Database = Database
  { query :: Text
  , schema :: Text
  }

data DatabaseAction
  = SetQuery Text
  | SetSchema Text
    deriving (Show, Typeable, Generic, NFData)

instance StoreData Database where
  type StoreAction Database = DatabaseAction
  transform (SetQuery query) database = pure database{query}
  transform (SetSchema schema) database = pure database{schema}

databaseStore :: ReactStore Database
databaseStore = mkStore Database { query = "SELECT 1;", schema = "" }
