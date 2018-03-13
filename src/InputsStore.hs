{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module InputsStore where

import Control.Concurrent.MVar
import Control.DeepSeq
import Data.IORef
import Data.Text
import Data.Typeable
import GHC.Generics
import React.Flux

import Catalog
import Dialects
import ResolvedStore (dialectRef, queryRef, schemaRef, triggerVar)

data Inputs = Inputs
    { dialect :: !SomeDialect
    , query :: !Text
    , schema :: !Text
    }

data InputsAction
    = SetDialect !SomeDialect
    | SetQuery !Text
    | SetSchema !Text
      deriving (Typeable, Generic, NFData)

instance StoreData Inputs where
  type StoreAction Inputs = InputsAction
  transform (SetDialect dialect) inputs = do
    writeIORef dialectRef dialect
    tryPutMVar triggerVar ()
    pure inputs{dialect}
  transform (SetQuery query) inputs = do
    writeIORef queryRef query
    tryPutMVar triggerVar ()
    pure inputs{query}
  transform (SetSchema schema) inputs = do
    writeIORef schemaRef schema
    tryPutMVar triggerVar ()
    pure inputs{schema}

inputsStore :: ReactStore Inputs
inputsStore = mkStore Inputs
    { dialect = SomeDialect (Proxy @Hive)
    , query = "SELECT 1;"
    , schema = defaultCatalog
    }
