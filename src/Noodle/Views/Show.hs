{-# LANGUAGE OverloadedStrings #-}

module Noodle.Views.Show where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.Monoid ((<>))

render (pollId, pollName, pollDesc) options = do
  H.html $ do
    H.body $ do
      H.h3 "Show of poll"
      H.h4 "Name:"
      H.p $ toHtml pollName
      H.h4 "Description"
      H.p $ toHtml pollDesc
      H.h4 "Options"
      H.ul $ do
        mapM_ renderLn options
      H.br
      H.legend "New option:"
      H.form ! A.method "post" ! A.action "/options/" $ do
        H.label "Name: "
        H.input ! A.name "name"
        H.input ! A.type_ "hidden" ! A.value (H.stringValue (show pollId)) !
          A.name "id"
        H.input ! A.type_ "submit" ! A.value "Add Options"
  where renderLn o = H.li $ H.toHtml $ (snd o)
