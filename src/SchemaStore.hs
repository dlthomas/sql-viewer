{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module SchemaStore where

import Control.DeepSeq
import Data.Text
import Data.Typeable
import GHC.Generics
import React.Flux

import Catalog

data Schema = Schema { schema :: !Text }

data SchemaAction = SetSchema !Text
    deriving (Show, Typeable, Generic, NFData)

instance StoreData Schema where
  type StoreAction Schema = SchemaAction
  transform (SetSchema schema) _ = pure Schema{schema}

schemaStore :: ReactStore Schema
schemaStore = mkStore Schema { schema = defaultCatalog }
