{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Tabs (tabs_) where

import Control.Monad
import Data.List
import Data.Text (Text)
import React.Flux

tabs_ :: [(Text, ReactElementM ViewEventHandler ())] -> ReactElementM handler ()
tabs_ ts = view (tabsView ts) () mempty

tabsView :: [(Text, ReactElementM ViewEventHandler ())] -> ReactView ()
tabsView [] = defineView "tabs" $ \ () -> div_ $ elemText "empty tab list"
tabsView tabs@((t, _):_) = defineStatefulView "tabs" t $ \ t () -> do
  div_ [classNames [("tab-list", True)]] $ do
    forM_ tabs $ \case
      (t', _)
        | t == t' -> div_ [classNames [("selected", True), ("tab", True)]] $ elemText t
        | otherwise -> div_ [classNames [("tab", True)], onClick $ \ _ _ _ -> ([], Just t')] $ elemText t'

  case lookup t tabs of
    Nothing -> pure ()
    Just widget -> div_ [classNames [("tab-body", True)]] $ liftViewToStateHandler widget
