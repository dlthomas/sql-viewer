{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module QueryParserView where

import Control.Monad (void, when)
import Control.Monad.Writer (runWriter)
import Data.Data
import Data.Foldable (forM_)
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Text.Lazy (fromStrict, intercalate, toStrict)
import React.Flux
import React.Flux.DOM

import Catalog
import QueryStore
import SchemaStore
import ResolvedStore
import Tabs

import Database.Sql.Hive.Parser (parseAll)
import Database.Sql.Position (Range)
import Database.Sql.Type
import Database.Sql.Util.Columns
import Database.Sql.Util.Eval (RecordSet(..))
import Database.Sql.Util.Lineage.ColumnPlus
import Database.Sql.Util.Lineage.Table

queryView :: ReactView ()
queryView = defineControllerView "query" queryStore $ \ (Query query) () -> do
  textarea_
    [ "value" &= query
    , onChange $ \ evt ->
        [SomeStoreAction queryStore $ SetQuery $ target evt "value"]
    ] mempty

schemaView :: ReactView ()
schemaView = defineControllerView "schema" schemaStore $ \ (Schema schema) () -> do
  textarea_
    [ "value" &= schema
    , onChange $ \ evt ->
        [SomeStoreAction schemaStore $ SetSchema $ target evt "value"]
    ] mempty

rawView :: ReactView ()
rawView = defineControllerView "raw" queryStore $ \ (Query query) () ->
  either elemShow renderAST $ parseAll $ fromStrict query

resolvedView :: ReactView ()
resolvedView = defineControllerView "resolved" resolvedStore $ \ (Resolved stmt) () -> either elemString renderAST stmt

columnsView :: ReactView ()
columnsView = defineControllerView "columns" resolvedStore $ \ (Resolved stmt) () ->
  case stmt of
    Left err -> elemString err
    Right stmt -> table_ $ do
      forM_ (getColumns stmt) $ \ (fqcn, clause) ->
        tr_ $ do
          td_ $ renderFQCN fqcn
          td_ $ elemText $ toStrict clause

renderFQCN :: FQCN -> ReactElementM handler ()
renderFQCN FullyQualifiedColumnName{..} = elemText $ toStrict $ intercalate "." [fqcnSchemaName, fqcnTableName, fqcnColumnName]

renderFQTN :: FQTN -> ReactElementM handler ()
renderFQTN FullyQualifiedTableName{..} = elemText $ toStrict $ intercalate "." [fqtnSchemaName, fqtnTableName]

renderColumnPlusSet :: ColumnPlusSet -> ReactElementM handler ()
renderColumnPlusSet ColumnPlusSet{..} =
  sequence_ $ intersperse (elemText "\n") $
       map renderFQTN (M.keys columnPlusTables)
    ++ map renderFQCN (M.keys columnPlusColumns)

columnLineageView :: ReactView ()
columnLineageView = defineControllerView "column-lineage" resolvedStore $ \ (Resolved resolved) () ->
  case resolved of
    Left err -> elemString err
    Right stmt -> table_ $ do
      tr_ $ do
        th_ $ pure ()
        th_ "Sources"
      case getColumnLineage stmt of
        (RecordSet{..}, effects) -> do
          let (columnSources, countSources) = runWriter recordSetItems
          when (mempty /= countSources) $ do
            tr_ $ do
              td_ "row count"
              td_ $ renderColumnPlusSet countSources
          forM_ (zip recordSetLabels columnSources) $ \ (column, sources) -> do
            tr_ $ do
              td_ $ case column of
                RColumnRef fqcn -> renderFQCN $ fqcnToFQCN fqcn
                RColumnAlias (ColumnAlias _ name _) -> elemText $ toStrict name
              td_ $ renderColumnPlusSet sources

          forM_ (M.toList effects) $ \ (target, sources) -> do
            tr_ $ do
              td_ $ either renderFQTN renderFQCN target
              td_ $ renderColumnPlusSet sources

tableLineageView :: ReactView ()
tableLineageView = defineControllerView "table-lineage" resolvedStore $ \ (Resolved resolved) () ->
  case resolved of
    Left err -> elemString err
    Right stmt -> table_ $ do
      tr_ $ do
        th_ $ pure ()
        th_ "Sources"
      forM_ (M.toList $ getTableLineage stmt) $ \ (target, sources) -> do
        td_ $ renderFQTN target
        td_ $ mapM_ renderFQTN sources

renderAST :: forall d handler. Data d => d -> ReactElementM handler ()
renderAST x
  | Just Refl <- eqT @d @Text
  = elemShow x
  | Just Refl <- eqT @d @String
  = elemShow x
  | dataIsList x
  = renderList x
  | "pack" == show (toConstr x)
  = void $ gmapM (\ y -> skip (li_ . renderAST) y >> pure y) x
  | otherwise
  = dl_ $ do
      dt_ $ elemShow (toConstr x)
      dd_ $ ul_ $ void $ gmapM (\ y -> skip (li_ . renderAST) y >> pure y) x

dataIsNothing :: forall d. Data d => d -> Bool
dataIsNothing x =
  typeRepTyCon (typeRep (Proxy @d)) == typeRepTyCon (typeRep (Proxy @(Maybe ())))
    && toConstr x == toConstr (Nothing :: Maybe ())

dataIsList :: forall d. Data d => d -> Bool
dataIsList x = typeRepTyCon (typeRep (Proxy @d)) == typeRepTyCon (typeRep (Proxy @([()])))

renderList :: forall d handler. Data d => d -> ReactElementM handler ()
renderList x
  | toConstr x == toConstr ([] :: [()])
  = elemText "[]"
  | otherwise
  = ol_ $ renderListItems x

data SomeData = forall d. Data d => SomeData d

renderListItems :: forall d handler. Data d => d -> ReactElementM handler ()
renderListItems x
  | toConstr x == toConstr ([] :: [()])
  = pure ()
  | [SomeData h, SomeData t] <- gmapQ SomeData x
  = do
    li_ $ renderAST h
    renderListItems t

skip :: forall a m. (Monad m, Data a) => (forall d. Data d => d -> m ()) -> a -> m ()
skip f x
  | Just Refl <- eqT @a @Range
  = pure ()
  | dataIsNothing x
  = pure ()
  | otherwise
  = f x

queryParserView :: ReactView ()
queryParserView = defineView "query parser" $ \ () -> do
  div_ [classNames [("frame", True)]] $ do
    tabs_
      [ ( "Query"
        , viewWithSKey queryView "query" () mempty
        )
      , ( "Schema"
        , viewWithSKey schemaView "schema" () mempty
        )
      ]
  div_ [classNames [("frame", True)]] $ tabs_
    [ ( "AST"
      , tabs_
          [ ( "Raw"
            , viewWithSKey rawView "raw-query" () mempty
            )
          , ( "Resolved"
            , viewWithSKey resolvedView "resolved-query" () mempty
            )
          ]
      )
    , ( "Columns"
      , viewWithSKey columnsView "columns" () mempty
      )
    , ( "Lineage"
      , tabs_
        [ ( "Table"
          , viewWithSKey tableLineageView "table-lineage" () mempty
          )
        , ( "Column (Plus Fields and Row Count)"
          , viewWithSKey columnLineageView "column-lineage" () mempty
          )
        ]
      )
    , ( "Evaluation"
      , elemText "stub"
      )
    ] 
