{-# LANGUAGE OverloadedStrings #-}

module Noodle.Views.Index where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.Monoid ((<>))

render items =
  H.html $ do
    H.head $ do
      H.title "Noodle - The doodle"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/noodle.css"
    H.body $ do
      H.h2 "Noodle - The doodle"
      H.a ! A.class_ "btn" ! A.href "/polls/new" $ "New Poll"
      H.table ! A.class_ "table" $ mapM_ renderLn items
  where renderLn i = H.tr $ H.td $
          H.a ! A.href ("/polls/" <> H.stringValue (show $ fst i)) $
            H.toHtml (snd i)
