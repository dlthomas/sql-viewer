{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module QueryParserView where

import Control.Monad (void)
import Data.Data
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import React.Flux
import React.Flux.DOM
import QueryStore
import SchemaStore
import Tabs

import Database.Sql.Hive.Parser (parseAll)
import Database.Sql.Position (Range)
import Database.Sql.Type

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
            , viewWithSKey rawView "query" () mempty
            )
          , ( "Resolved"
            , elemText "stub"
            )
          ]
      )
    , ( "Columns"
      , elemText "stub"
      )
    , ( "Lineage"
      , elemText "stub"
      )
    , ( "Evaluation"
      , elemText "stub"
      )
    ] 
