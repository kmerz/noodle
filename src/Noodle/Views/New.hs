{-# LANGUAGE OverloadedStrings #-}

module Noodle.Views.New where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.Monoid ((<>))

render errors =
  H.html $ do
    H.head $ do
      H.title "Noodle - The doodle"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/noodle.css"
    H.body $ do
      H.h2 ! A.class_ "header" $ "Noodle - The doodle"
      H.h3 ! A.class_ "title" $ "Create a poll"
      H.div ! A.class_ "container" $ do
        mapM_ renderErrors errors
        H.form ! A.class_ "form" ! A.method "post" ! A.action "/polls/" $ do
          H.table $ do
            H.tr $ do
              H.td $ H.label "Name: "
              H.td $ H.input ! A.name "name"
            H.tr $ do
              H.td $ H.label "Description: "
              H.td $ H.textarea ! A.name "desc" ! A.cols "50" ! A.rows "10" $ ""
          H.div ! A.class_ "btns" $ do
            H.a ! A.class_ "btn" ! A.href "/polls" $ "Back"
            H.input ! A.class_ "btn" ! A.type_ "submit" ! A.value "Add Poll"
  where renderErrors error = do
          H.p ! A.class_ "error" $ error
          H.br
