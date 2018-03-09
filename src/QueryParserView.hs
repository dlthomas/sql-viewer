{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module QueryParserView where

import React.Flux
import React.Flux.DOM
import QueryStore
import SchemaStore
import Tabs

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

queryParserView :: ReactView ()
queryParserView = defineView "query parser" $ \ () -> do
  div_ [classNames [("frame", True)]] $ tabs_
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
            , elemText "stub"
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
