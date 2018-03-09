{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module QueryStore where

import Control.Concurrent.MVar
import Control.DeepSeq
import Data.IORef
import Data.Text
import Data.Typeable
import GHC.Generics
import React.Flux

import ResolvedStore (queryRef, triggerVar)

data Query = Query { query :: !Text }

data QueryAction = SetQuery !Text
    deriving (Show, Typeable, Generic, NFData)

instance StoreData Query where
  type StoreAction Query = QueryAction
  transform (SetQuery query) _ = do
    writeIORef queryRef query
    tryPutMVar triggerVar ()
    pure Query{query}

queryStore :: ReactStore Query
queryStore = mkStore Query { query = "SELECT 1;" }
