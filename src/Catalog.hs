{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Catalog where

import Data.Aeson as JSON
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HMS
import Data.Text as T (Text, unlines)
import Data.Text.Lazy as TL (fromStrict, pack, toStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Typeable (typeRep)

import Dialects

import Database.Sql.Type

parseCatalog :: SomeDialect -> Text -> Text -> Either String Catalog
parseCatalog (SomeDialect dialect) schema path = do
  let database = DatabaseName () $ TL.pack $ show $ typeRep dialect
      decode :: FromJSON a => Text -> Either String a
      decode = JSON.eitherDecode' . encodeUtf8 . fromStrict
  schema' <- decode schema
  path' <- map (`mkNormalSchema` ()) <$> decode path
  pure $ makeCatalog (HMS.singleton database schema') path' database

defaultCatalog :: Text
defaultCatalog = T.unlines
  [ "{\"public\":"
  , "    {\"tbl\": [\"a\", \"b\"]}}"
  ]

instance FromJSON DatabaseMap where
  parseJSON = withObject "database-map" $
    (HMS.fromList <$>) . mapM (\ (k, v) -> (mkNormalSchema (fromStrict k) (),) <$> parseJSON v) . HMS.toList

instance FromJSON SchemaMap where
  parseJSON = withObject "schema-map" $
    (HMS.fromList <$>) . mapM (\ (k, v) -> (QTableName () None $ fromStrict k,) <$> parseJSON v) . HMS.toList

instance FromJSON SchemaMember where
  parseJSON = withArray "table" $ \ array -> do
    columns <- toList <$> mapM parseColumn array
    pure $ SchemaMember Table Persistent columns Nothing
    where
      parseColumn = withText "column" $ pure . QColumnName () None . fromStrict
