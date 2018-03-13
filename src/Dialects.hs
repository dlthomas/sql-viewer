{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Dialects where

import Control.Arrow
import Data.Data
import Data.Text.Lazy (Text)
import Database.Sql.Hive.Parser as Hive
import Database.Sql.Hive.Type
import Database.Sql.Position (Range)
import Database.Sql.Type
import Database.Sql.Util.Scope

class KnownDialect d where
    type RawAST d = raw | raw -> d
    type ResolvedAST d = resolved | resolved -> d
    parse :: Text -> Either String (RawAST d)
    resolve :: Catalog -> RawAST d -> Either String (ResolvedAST d)

instance KnownDialect Hive where
    type RawAST Hive = HiveStatement RawNames Range
    type ResolvedAST Hive = HiveStatement ResolvedNames Range
    parse = left show . Hive.parseAll
    resolve catalog stmt = left show $ runResolverNoWarn (resolveHiveStatement stmt) (Proxy :: Proxy Hive) catalog
