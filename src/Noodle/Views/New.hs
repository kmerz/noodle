{-# LANGUAGE OverloadedStrings #-}

module Noodle.Views.New where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.Monoid ((<>))

render errors = do
  H.html $ do
    H.head $ do
      H.title "Noodle - The doodle"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/noodle.css"
    H.body $ do
      H.h2 "Create a poll"
      mapM_ renderErrors errors
      H.form ! A.class_ "form" ! A.method "post" ! A.action "/polls/" $ do
        H.table $ do
          H.tr $ do
            H.td $ do
              H.label "Name: "
            H.td $ do
              H.input ! A.name "name"
          H.tr $ do
            H.td $ do
              H.label "Description: "
            H.td $ do
              H.textarea ! A.name "desc" ! A.cols "50" ! A.rows "10" $ ""
        H.div ! A.class_ "btns" $ do
          H.input ! A.class_ "btn" ! A.type_ "submit" ! A.value "Add Poll"
          H.a ! A.class_ "btn" ! A.href "/polls" $ "Back"
  where renderErrors error = do
          H.p ! A.class_ "error" $ error
          H.br
