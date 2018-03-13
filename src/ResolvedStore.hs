{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ResolvedStore where

import Control.Arrow (left)
import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Monad
import Data.IORef
import Data.Text
import Data.Text.Lazy (fromStrict)
import Data.Typeable
import GHC.Generics
import React.Flux
import System.IO.Unsafe

import Catalog
import Dialects

import Database.Sql.Hive.Parser (parseAll)
import Database.Sql.Hive.Type
import Database.Sql.Position
import Database.Sql.Type.Scope
import Database.Sql.Util.Scope


data Resolved = Resolved { resolved :: Either String (HiveStatement ResolvedNames Range) }

data ResolvedAction = SetResolved (Either String (HiveStatement ResolvedNames Range))
    deriving (Typeable, Generic)

instance StoreData Resolved where
  type StoreAction Resolved = ResolvedAction
  transform (SetResolved resolved) _ = pure Resolved{resolved}

resolvedStore :: ReactStore Resolved
resolvedStore = mkStore Resolved { resolved = Left "initializing" }

queryRef :: IORef Text
queryRef = unsafePerformIO $ newIORef "SELECT 1;"

schemaRef :: IORef Text
schemaRef = unsafePerformIO $ newIORef defaultCatalog

triggerVar :: MVar ()
triggerVar = unsafePerformIO newEmptyMVar

resolverThread :: IO ()
resolverThread = forever $ do
  query <- readIORef queryRef
  schema <- readIORef schemaRef
  let resolved = do
        raw <- left show $ parse @Hive (fromStrict query)
        catalog <- parseCatalog schema
        resolve catalog raw
  alterStore resolvedStore $ SetResolved resolved
  takeMVar triggerVar
