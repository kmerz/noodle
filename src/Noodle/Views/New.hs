{-# LANGUAGE OverloadedStrings #-}

module Noodle.Views.New where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.Monoid ((<>))

render = do
  H.html $ do
    H.body $ do
      H.legend "Create a poll"
      H.form ! A.method "post" ! A.action "/polls/" $ do
        H.label "Name: "
        H.input ! A.name "name"
        H.br
        H.label "Description: "
        H.input ! A.name "desc"
        H.br
        H.input ! A.type_ "submit" ! A.value "Add Poll"
