{-# LANGUAGE OverloadedStrings #-}

module Noodle.Views.Show where

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text
import Data.Monoid ((<>))

render poll = do
  html $ do
    body $ do
      h3 "Show of poll"
      h5 "Name:"
      p $ toHtml $ fst poll
      h5 "Description"
      p $ toHtml $ snd poll
