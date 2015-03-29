{-# LANGUAGE OverloadedStrings #-}

module Noodle.Views.Show where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

render (pollId, pollName, pollDesc) options voters errors = do
  H.html $ do
    H.body $ do
      H.h3 $ toHtml pollName
      mapM_ (\x-> do H.toHtml x; H.br) (lines pollDesc)
      H.h4 "Options"
      mapM_ renderErrors errors
      H.form ! A.method "post" ! A.action "/options/" $ do
        H.input ! A.placeholder "add a option" ! A.name "name"
        H.input ! A.type_ "hidden" ! A.value (H.stringValue (show pollId)) !
          A.name "id"
        H.input ! A.type_ "submit" ! A.value "Add"
      H.form ! A.method "post" ! A.enctype "application/json" !
        A.action (H.stringValue ("/polls/" ++ (show pollId) ++ "/vote/")) $ do
        H.table $ do
          mapM_ (renderLn) (zip options voters)
        H.input ! A.placeholder "Vote as" ! A.name "name"
        H.input ! A.type_ "submit" ! A.value "Vote"
      H.br
      H.a ! A.href "/polls" $ "Back"
  where renderLn ((id, name), (_, voters)) = do
          H.tr $ do
            H.td $ do
              H.p $ H.toHtml $ (show (length voters))
            H.td $ do
              H.input ! A.name "option_id" ! A.value (H.stringValue (show id)) !
                A.type_ "checkbox"
            H.td $ do
              H.b $ H.toHtml $ name
            H.td $ do
              H.p $ H.toHtml $ unwords $ Prelude.map (\x -> x ++ ", ") voters
        renderErrors error = do
          H.p ! A.style "color: red" $ error
          H.br
