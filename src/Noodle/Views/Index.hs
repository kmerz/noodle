{-# LANGUAGE OverloadedStrings #-}

module Noodle.Views.Index where

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text
import Data.Monoid ((<>))

render items = do
  html $ do
    body $ do
      h2 "Index of polls"
      a ! href "/polls/new" $ "New Poll"
      ul $ do
        mapM_ renderLn items
  where renderLn i = li (a ! href ("/polls/" <> (stringValue (show $ fst i))) $
                                toHtml (snd i))
