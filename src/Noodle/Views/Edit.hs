{-# LANGUAGE OverloadedStrings #-}

module Noodle.Views.Edit where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

render (pollId, pollName, pollDesc) options errors =
  H.html $ do
    H.head $ do
      H.title "Noodle - The doodle"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/noodle.css"
    H.body $ do
      H.h2 ! A.class_ "header" $ "Noodle - The doodle"
      H.h3 ! A.class_ "title" $ toHtml $ "Edit options of " ++ pollName
      H.div ! A.class_ "container" $ do
        mapM_ renderErrors errors
        H.form ! A.method "post" ! A.action "/options/" $ do
          H.input ! A.class_ "input" ! A.placeholder "add a option" !
            A.name "name"
          H.input ! A.class_ "input" ! A.placeholder "with description" !
            A.name "desc"
          H.input ! A.type_ "hidden" ! A.value (H.stringValue (show pollId)) !
            A.name "id"
          H.input ! A.class_ "btn" ! A.type_ "submit" ! A.value "Add"
        H.form ! A.method "post" !
          A.action (H.stringValue "/options/delete") $ do
          H.table $ mapM_ renderLn options
          H.input ! A.type_ "hidden" ! A.value (H.stringValue (show pollId)) !
            A.name "id"
          H.input ! A.class_ "btn" ! A.type_ "submit" ! A.value "Delete"
          H.a ! A.class_ "btn" !
            A.href (H.stringValue ("/polls/" ++ show pollId)) $ "To poll"
  where renderErrors error = do
          H.p ! A.class_ "error" $ error
          H.br
        renderLn (id, name, desc) =
          H.tr $ do
            H.td $
              H.input ! A.name "option_id" ! A.value (H.stringValue (show id)) !
                A.type_ "checkbox"
            H.td $ H.b $ H.toHtml name
            H.td $ H.toHtml desc
