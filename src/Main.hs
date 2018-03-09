module Main where

import QueryParserView
import React.Flux

main :: IO ()
main = reactRender "main" queryParserView ()
