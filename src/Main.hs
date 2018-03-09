module Main where

import Control.Concurrent (forkIO)
import React.Flux

import ResolvedStore
import QueryParserView

main :: IO ()
main = do
  forkIO resolverThread
  reactRender "main" queryParserView ()
