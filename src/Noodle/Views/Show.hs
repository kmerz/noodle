{-# LANGUAGE OverloadedStrings #-}

module Noodle.Views.Show where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.Monoid ((<>))

render (pollId, pollName, pollDesc) options voters = do
  H.html $ do
    H.body $ do
      H.h3 $ toHtml pollName
      H.p $ toHtml pollDesc
      H.h4 "Options"
      H.form ! A.method "post" ! A.enctype "application/json" !
        A.action (H.stringValue ("/polls/" ++ (show pollId) ++ "/vote/")) $ do
        H.table $ do
          mapM_ (renderLn) (zip options voters)
        H.input ! A.placeholder "Vote as" ! A.name "name"
        H.input ! A.type_ "submit" ! A.value "Vote"
      H.br
      H.form ! A.method "post" ! A.action "/options/" $ do
        H.input ! A.name "name"
        H.input ! A.type_ "hidden" ! A.value (H.stringValue (show pollId)) !
          A.name "id"
        H.input ! A.type_ "submit" ! A.value "Add Options"
      H.a ! A.href "/polls" $ "Back"
  where renderLn ((id, name), (_, voters)) = do
          H.tr $ do
            H.td $ do
              H.input ! A.name "option_id" ! A.value (H.stringValue (show id)) !
                A.type_ "checkbox"
            H.td $ do
              H.toHtml $ name
            H.td $ do
              H.p $ H.toHtml $ unwords voters
