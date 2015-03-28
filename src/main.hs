{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans (liftIO)
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Database.HDBC
import Database.HDBC.Sqlite3

import qualified Noodle.Views.Index

blaze = S.html . renderHtml

main = S.scotty 3000 $ do
  S.get "/" $ do
    blaze $ Noodle.Views.Index.render [ "Konrad" ]
