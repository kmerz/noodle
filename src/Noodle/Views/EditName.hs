{-# LANGUAGE OverloadedStrings #-}

module Noodle.Views.EditName where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.Monoid ((<>))

render (pollId, pollName, pollDesc) errors = do
  H.html $ do
    H.head $ do
      H.title "Noodle - The doodle"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/noodle.css"
    H.body $ do
      H.h2 "Edit the poll"
      mapM_ renderErrors errors
      H.form ! A.class_ "form" ! A.method "post" !
        A.action (H.stringValue ("/polls/" ++ (show pollId) ++ "/update")) $ do
        H.table $ do
          H.tr $ do
            H.td $ do
              H.label "Name: "
            H.td $ do
              H.input ! A.name "name" ! A.value (H.stringValue pollName)
          H.tr $ do
            H.td $ do
              H.label "Description: "
            H.td $ do
              H.textarea ! A.name "desc" ! A.cols "50" ! A.rows "10" $
                H.toHtml pollDesc
        H.div ! A.class_ "btns" $ do
          H.input ! A.class_ "btn" ! A.type_ "submit" ! A.value "Update Poll"
      H.a ! A.class_ "btn" ! A.href (H.stringValue ("/polls/" ++ (show pollId))) $
        "To poll"
  where renderErrors error = do
          H.p ! A.class_ "error" $ error
          H.br
