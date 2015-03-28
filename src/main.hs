{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import Control.Monad.Trans (liftIO)
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)
import Data.Time

import qualified Noodle.Views.Index

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Poll
    name String
    desc String
    deriving Show
  |]


blaze = S.html . renderHtml

initDb = do
  runSqlite "noodle.db" $ do
    runMigration migrateAll
    insert $ Poll "What should we do?" "It is about time to do somthing"
    insert $ Poll "What should we eat?" "It is about time to eat somthing"

allPolls = do
  runSqlite "noodle.db" $ do
    polls <- selectList ([] :: [Filter Poll]) []
    return polls


main :: IO ()
main = do
  initDb
  scottySite

scottySite = do
  S.scotty 3000 $ do
    polls <- liftIO $ allPolls
    S.get "/" $ do
      blaze $ Noodle.Views.Index.render [ H.toHtml (show polls) ]
