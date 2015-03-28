{-# LANGUAGE OverloadedStrings #-}

module Noodle.Views.Index where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

render items = do
  H.html $ do
    H.body $ do
      H.h1 "My todo list"
      H.ul $ do
        H.li (head items)
        H.li (head items)
