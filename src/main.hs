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
import Data.Text.Lazy as T (pack)
import Data.Monoid (mconcat)

import qualified Noodle.Views.Index
import qualified Noodle.Views.Show
import qualified Noodle.Views.New

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Poll
    name String
    desc String
    deriving Show
  Option
    pollId PollId
    name String
    deriving Show
  |]

blaze = S.html . renderHtml
main :: IO ()
main = do
  initDb
  scottySite

scottySite = do
  S.scotty 3000 $ do
    S.get "/polls" $ do
      polls <- liftIO $ allPolls
      blaze $ Noodle.Views.Index.render $ pollNames $ polls
    S.get "/" $ do
      polls <- liftIO $ allPolls
      blaze $ Noodle.Views.Index.render $ pollNames $ polls
    S.get "/polls/new" $ do
      blaze $ Noodle.Views.New.render
    S.get "/polls/:id" $ do
      id <- S.param "id"
      poll <- liftIO $ getPollById id
      options <- liftIO $ getOptionsByPollId id
      blaze $ Noodle.Views.Show.render (pollValues $ head poll)
        (optionsValues options)
    S.post "/polls/" $ do
      name <- S.param "name"
      desc <- S.param "desc"
      createPoll name desc
      S.redirect "/"
    S.post "/options/" $ do
      name <- S.param "name" :: S.ActionM String
      pId <- S.param "id":: S.ActionM String
      createOption pId name
      S.redirect $ T.pack $ "/polls/" ++ pId

initDb = do
  runSqlite "noodle.db" $ do
    runMigration migrateAll

createPoll name desc = do
  runSqlite "noodle.db" $ do
    insert $ Poll name desc

allPolls = do
  runSqlite "noodle.db" $ do
    polls <- selectList ([] :: [Filter Poll]) []
    return $ polls

getPollById id = do
  runSqlite "noodle.db" $ do
    selectList [PollId ==. (toSqlKey (read id))] [LimitTo 1]

pollNames = map (\i -> ((getPollId i), (pollName (entityVal i))))

getPollId x = unSqlBackendKey $ unPollKey $ entityKey x

pollValues i = ((getPollId i), (pollName (entityVal i)), (pollDesc (entityVal i)))

optionsValues = map (\o -> ((getOptionId o),
  (optionName (entityVal o))))

getOptionsByPollId id = do
  runSqlite "noodle.db" $ do
    selectList [OptionPollId ==. (toSqlKey (read id))] []

getOptionId x = unSqlBackendKey $ unOptionKey $ entityKey x

createOption pId name = do
  runSqlite "noodle.db" $ do
    insert $ Option (toSqlKey (read pId)) name
