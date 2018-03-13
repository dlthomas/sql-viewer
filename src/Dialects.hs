{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Dialects (KnownDialect(..), SomeDialect(..), Hive, Presto, Vertica) where

import Control.Arrow
import Control.DeepSeq
import Data.Data
import Data.Text.Lazy (Text)
import Database.Sql.Position (Range)
import Database.Sql.Type
import Database.Sql.Util.Scope
import Database.Sql.Util.Columns
import Database.Sql.Util.Lineage.ColumnPlus
import Database.Sql.Util.Lineage.Table

import Database.Sql.Hive.Parser as Hive
import Database.Sql.Hive.Type
import Database.Sql.Presto.Parser as Presto
import Database.Sql.Presto.Type
import Database.Sql.Vertica.Parser as VSQL
import Database.Sql.Vertica.Type

class
  ( Data (RawAST d), Data (ResolvedAST d)
  , HasColumns (ResolvedAST d)
  , HasColumnLineage (ResolvedAST d)
  , HasTableLineage (ResolvedAST d)
  ) => KnownDialect d where
    type RawAST d = raw | raw -> d
    type ResolvedAST d = resolved | resolved -> d
    parse :: Text -> Either String (RawAST d)
    resolve :: Catalog -> RawAST d -> Either String (ResolvedAST d)

instance KnownDialect Hive where
    type RawAST Hive = HiveStatement RawNames Range
    type ResolvedAST Hive = HiveStatement ResolvedNames Range
    parse = left show . Hive.parseAll
    resolve catalog stmt = left show $ runResolverNoWarn (resolveHiveStatement stmt) (Proxy :: Proxy Hive) catalog

instance KnownDialect Presto where
    type RawAST Presto = PrestoStatement RawNames Range
    type ResolvedAST Presto = PrestoStatement ResolvedNames Range
    parse = left show . Presto.parseAll
    resolve catalog stmt = left show $ runResolverNoWarn (resolvePrestoStatement stmt) (Proxy :: Proxy Presto) catalog

instance KnownDialect Vertica where
    type RawAST Vertica = VerticaStatement RawNames Range
    type ResolvedAST Vertica = VerticaStatement ResolvedNames Range
    parse = left show . VSQL.parseAll
    resolve catalog stmt = left show $ runResolverNoWarn (resolveVerticaStatement stmt) (Proxy :: Proxy Vertica) catalog

data SomeDialect = forall d. (Typeable d, KnownDialect d) => SomeDialect (Proxy d)

instance Eq SomeDialect where
  SomeDialect x == SomeDialect y = typeRep x == typeRep y

instance NFData SomeDialect where
    rnf (SomeDialect d) = d `seq` ()
