{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
  | otherwise
  = dl_ $ do
      dt_ $ elemShow (toConstr x)
      dd_ $ ul_ $ void $ gmapM (\ y -> li_ (renderAST y) >> pure y) x

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
