{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module QueryParserView where

import React.Flux
import React.Flux.DOM
import DatabaseStore
import Tabs

queryParserView :: ReactView ()
queryParserView = defineControllerView "query parser" databaseStore $ \ Database{query} () ->
  div_ $ do
    div_ $ tabs_
      [ ( "Query"
        , input_
            [ "value" &= query
            , onChange $ \ evt ->
                [SomeStoreAction databaseStore $ SetQuery $ target evt "value"]
            ]
        )
      , ( "Schema"
        , elemText "stub"
        )
      ]
    div_ $ tabs_
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
