{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Catalog where

import Data.Aeson as JSON
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HMS
import Data.Text (Text)
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)

import Database.Sql.Type

parseCatalog :: Text -> Either String Catalog
parseCatalog = JSON.eitherDecode' . encodeUtf8 . fromStrict

defaultCatalog :: Text
defaultCatalog = mconcat
  [ "{ \"map\":"
  , "    {\"hive\":"
  , "        {\"public\":"
  , "           {\"tbl\":[\"a\"]}}}"
  , ", \"path\": [\"public\"]"
  , ", \"current\": \"hive\"}"
  ]

instance FromJSON Catalog where
  parseJSON = withObject "catalog" $ \ o ->
    makeCatalog
      <$> o .: "map"
      <*> (map (`mkNormalSchema` ()) <$> o .: "path")
      <*> (DatabaseName () <$> o .: "current")

instance FromJSON CatalogMap where
  parseJSON = withObject "catalog-map" $
    (HMS.fromList <$>) . mapM (\ (k, v) -> (DatabaseName () $ fromStrict k,) <$> parseJSON v) . HMS.toList

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
