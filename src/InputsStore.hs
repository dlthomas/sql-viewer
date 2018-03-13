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
import ResolvedStore (queryRef, schemaRef, triggerVar)

data Inputs = Inputs
    { query :: !Text
    , schema :: !Text
    }

data InputsAction
    = SetQuery !Text
    | SetSchema !Text
      deriving (Show, Typeable, Generic, NFData)

instance StoreData Inputs where
  type StoreAction Inputs = InputsAction
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
    { query = "SELECT 1;"
    , schema = defaultCatalog
    }
