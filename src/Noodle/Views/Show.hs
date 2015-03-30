{-# LANGUAGE OverloadedStrings #-}

module Noodle.Views.Show where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.List (intercalate)
import Data.Map as M hiding ((!))

render (pollId, pollName, pollDesc) options voters cants errors editVoter = do
  H.html $ do
    H.head $ do
      H.title "Noodle - The doodle"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/noodle.css"
    H.body $ do
      H.h3 $ toHtml pollName
      mapM_ (\x-> do H.toHtml x; H.br) (lines pollDesc)
      H.br
      H.a ! A.class_ "btn" ! A.href (
        H.stringValue ("/polls/" ++ (show pollId) ++ "/edit_name")) $ "Edit"
      mapM_ renderErrors errors
      H.table $ do
        H.tr $ do
          H.td ""
          mapM_ renderOption options
        mapM_ renderVoter $ M.keys voters
        mapM_ renderCants cants
        H.tr $ do
          H.td $ ""
          mapM_ renderVoteCount options
        H.tr $ do
          H.form ! A.method "post" !
            A.action (H.stringValue ("/polls/" ++ (show pollId) ++ "/vote")) $ do
            H.td $ do
              H.input ! A.class_ "input" ! A.placeholder "Vote as" ! A.name "name"
            mapM_ renderCheckbox options
            H.td $ H.input ! A.class_ "btn" ! A.type_ "submit" ! A.value "Vote"
      H.br
      H.a ! A.class_ "btn" ! A.href "/polls" $ "To overview"
      H.a ! A.class_ "btn" !
        A.href (H.stringValue ("/polls/" ++ (show pollId) ++ "/edit")) $
          "Edit Options"
  where renderOption (id, name, desc) = do
          H.td $ do
            H.b $ H.toHtml $ name
            H.br
            H.toHtml desc
        renderCheckbox (id, _, _) = do
          H.td ! A.class_ "checker" $ do
            H.input ! A.name "option_id" !
              A.value (H.stringValue (show id)) !
              A.type_ "checkbox"
        renderCants cant = do
          if cant == editVoter
            then renderEditVote cant
            else
            H.tr $ do
              H.td $ H.toHtml cant
              mapM_ (\ (_, _, _) ->
                H.td ! A.class_ "false" $ "") options
              H.td $ H.a ! A.class_ "btn" !
                A.href (H.stringValue (
                  "/polls/" ++ (show pollId) ++ "/vote/" ++ cant ++ "/edit")) $
                    "Edit"
              H.td $ H.a ! A.class_ "btn" !
                A.href (H.stringValue (
                  "/polls/" ++ (show pollId) ++ "/vote/" ++ cant ++ "/delete")) $
                    "Delete"
        renderVoter voter = do
          if (voter == editVoter)
            then
              renderEditVote voter
            else
            H.tr $ do
              H.td $ H.toHtml voter
              mapM_ (\ (id, _, _) ->
                case M.lookup voter voters of
                  Just ids -> do if (id `elem` ids)
                                   then H.td ! A.class_ "true" $ "✓"
                                   else H.td ! A.class_ "false" $ ""
                  Nothing -> H.td ""
                ) options
              H.td $ H.a ! A.class_ "btn" !
                A.href (H.stringValue (
                  "/polls/" ++ (show pollId) ++ "/vote/" ++ voter ++ "/edit")) $
                    "Edit"
              H.td $ H.a ! A.class_ "btn" !
                A.href (H.stringValue (
                  "/polls/" ++ (show pollId) ++ "/vote/" ++ voter ++ "/delete")) $
                    "Delete"
        renderVoteCount (id, _, _) = do
          H.td ! A.class_ "count" $ H.toHtml (show count)
          where count = M.fold(\ids acc ->
                  if id `elem` ids then acc + 1 else acc) 0 voters
        renderErrors error = do
          H.p ! A.class_ "error" $ error
          H.br
        renderEditVote voter = do
          H.tr $ do
            H.form ! A.method "post" !
              A.action (H.stringValue (
                "/polls/" ++ (show pollId) ++ "/vote")) $ do
              H.td $ do
                H.input ! A.class_ "input" ! A.disabled "disabled" !
                  A.name "name-disabled" ! A.value (H.stringValue voter)
                H.input ! A.class_ "input" ! A.type_ "hidden" !
                  A.name "name" ! A.value (H.stringValue voter)
              mapM_ renderCheckbox options
              H.td $ H.input ! A.class_ "btn" ! A.type_ "submit" !
                A.value "Update"
