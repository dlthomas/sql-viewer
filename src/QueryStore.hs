{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module QueryStore where

import Control.DeepSeq
import Data.Text
import Data.Typeable
import GHC.Generics
import React.Flux

data Query = Query { query :: !Text }

data QueryAction = SetQuery !Text
    deriving (Show, Typeable, Generic, NFData)

instance StoreData Query where
  type StoreAction Query = QueryAction
  transform (SetQuery query) _ = pure Query{query}

queryStore :: ReactStore Query
queryStore = mkStore Query { query = "SELECT 1;" }
