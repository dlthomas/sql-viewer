{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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


data Resolved = forall d. KnownDialect d => Resolved { resolved :: Either String (ResolvedAST d) }

data ResolvedAction = forall d. KnownDialect d => SetResolved (Either String (ResolvedAST d))
    deriving (Typeable)

instance StoreData Resolved where
  type StoreAction Resolved = ResolvedAction
  transform (SetResolved resolved) _ = pure Resolved{resolved}

resolvedStore :: ReactStore Resolved
resolvedStore = mkStore Resolved { resolved = Left "initializing" :: Either String (ResolvedAST Hive) }

dialectRef :: IORef SomeDialect
dialectRef = unsafePerformIO $ newIORef $ SomeDialect (Proxy @Hive)

queryRef :: IORef Text
queryRef = unsafePerformIO $ newIORef "SELECT 1;"

schemaRef :: IORef Text
schemaRef = unsafePerformIO $ newIORef defaultCatalog

triggerVar :: MVar ()
triggerVar = unsafePerformIO newEmptyMVar

resolverThread :: IO ()
resolverThread = forever $ do
  SomeDialect (_ :: Proxy dialect) <- readIORef dialectRef
  query <- readIORef queryRef
  schema <- readIORef schemaRef
  let resolved = do
        raw <- left show $ parse @dialect (fromStrict query)
        catalog <- parseCatalog schema
        resolve catalog raw
  alterStore resolvedStore $ SetResolved resolved
  takeMVar triggerVar
