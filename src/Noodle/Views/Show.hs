{-# LANGUAGE OverloadedStrings #-}

module Noodle.Views.Show where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.List (intercalate)

render (pollId, pollName, pollDesc) options voters errors = do
  H.html $ do
    H.head $ do
      H.title "Noodle - The doodle"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/noodle.css"
    H.body $ do
      H.h3 $ toHtml pollName
      mapM_ (\x-> do H.toHtml x; H.br) (lines pollDesc)
      H.h4 "Options"
      mapM_ renderErrors errors
      H.form ! A.method "post" ! A.action "/options/" $ do
        H.input ! A.class_ "input" ! A.placeholder "add a option" ! A.name "name"
        H.input ! A.class_ "input" ! A.placeholder "with desctiption" ! A.name "desc"
        H.input ! A.type_ "hidden" ! A.value (H.stringValue (show pollId)) !
          A.name "id"
        H.input ! A.class_ "btn" ! A.type_ "submit" ! A.value "Add"
      H.form ! A.method "post" !
        A.action (H.stringValue ("/polls/" ++ (show pollId) ++ "/vote/")) $ do
        H.table $ do
          mapM_ (renderLn) (zip options voters)
        H.input ! A.class_ "input" ! A.placeholder "Vote as" ! A.name "name"
        H.input ! A.class_ "btn" ! A.type_ "submit" ! A.value "Vote"
      H.br
      H.a ! A.class_ "btn" ! A.href "/polls" $ "Back"
  where renderLn ((id, name, desc), (_, voters)) = do
          H.tr $ do
            H.td $ do
              H.p $ H.toHtml $ (show (length voters))
            H.td $ do
              H.input ! A.name "option_id" ! A.value (H.stringValue (show id)) !
                A.type_ "checkbox"
            H.td $ do
              H.b $ H.toHtml $ name
              H.br
              H.toHtml desc
            H.td ! A.class_ "voters" $ do
              H.p $ H.toHtml $ intercalate ", " voters
        renderErrors error = do
          H.p ! A.class_ "error" $ error
          H.br
