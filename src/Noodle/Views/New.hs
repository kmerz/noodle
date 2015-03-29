{-# LANGUAGE OverloadedStrings #-}

module Noodle.Views.New where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.Monoid ((<>))

render errors = do
  H.html $ do
    H.body $ do
      H.h3 "Create a poll"
      mapM_ renderErrors errors
      H.form ! A.method "post" ! A.action "/polls/" $ do
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
        H.input ! A.type_ "submit" ! A.value "Add Poll"
  where renderErrors error = do
          H.p ! A.style "color: red" $ error
          H.br
