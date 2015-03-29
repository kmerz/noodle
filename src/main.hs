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
import Control.Monad (filterM)
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)
import Data.Time
import Data.Text.Lazy as T (unpack, pack)
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
    desc String
    deriving Show
  Vote
    optionId OptionId
    voter String
    UniqueVote optionId voter
    deriving Show
  |]

blaze = S.html . renderHtml
main :: IO ()
main = do
  initDb
  scottySite

scottySite = do
  S.scotty 3000 $ do
    S.get "/noodle.css" $ do
      S.file "noodle.css"
    S.get "/polls" $ do
      polls <- liftIO $ allPolls
      blaze $ Noodle.Views.Index.render $ pollNames $ polls
    S.get "/" $ do
      polls <- liftIO $ allPolls
      blaze $ Noodle.Views.Index.render $ pollNames $ polls
    S.get "/polls/new" $ do
      blaze $ Noodle.Views.New.render []
    S.post "/polls/:id/vote" $ do
      id <- S.param "id" :: S.ActionM String
      poll <- liftIO $ getPollById id
      name <- S.param "name" :: S.ActionM String
      options <- liftIO $ getOptionsByPollId id
      voters <- liftIO $ getVotersByOptionIds (optionIds options)
      all_params <- S.params
      let choosen_opt_ids = foldl (\ acc (key, value) -> if key == "option_id"
          then (T.unpack value):acc
          else acc) [] all_params

      case name of
        "" -> blaze $ Noodle.Views.Show.render (pollValues $ head poll)
                (optionsValues options) (voterValues voters) ["Vote needs a name"]
        otherwise -> do
          voteForOptions name (optionIds options) choosen_opt_ids
          S.redirect $ T.pack $ "/polls/" ++ id
    S.get "/polls/:id" $ do
      id <- S.param "id"
      poll <- liftIO $ getPollById id
      options <- liftIO $ getOptionsByPollId id
      voters <- liftIO $ getVotersByOptionIds (optionIds options)
      blaze $ Noodle.Views.Show.render (pollValues $ head poll)
        (optionsValues options) (voterValues voters) []
    S.post "/polls/" $ do
      name <- S.param "name"
      desc <- S.param "desc"
      case name of
        "" -> blaze $ Noodle.Views.New.render [ "Poll needs a name" ]
        otherwise -> do
          newId <- liftIO $ createPoll name desc
          S.redirect $ T.pack $ "/polls/" ++ (show (getNewPollId newId))
    S.post "/options/" $ do
      name <- S.param "name" :: S.ActionM String
      desc <- S.param "desc" :: S.ActionM String
      pId <- S.param "id":: S.ActionM String
      poll <- liftIO $ getPollById pId
      options <- liftIO $ getOptionsByPollId pId
      voters <- liftIO $ getVotersByOptionIds (optionIds options)
      case name of
        "" -> do blaze $ Noodle.Views.Show.render (pollValues $ head poll)
                   (optionsValues options) (voterValues voters)
                   [ "Option needs a name" ]
        otherwise -> do
          createOption pId name desc
          S.redirect $ T.pack $ "/polls/" ++ pId

initDb = do
  runSqlite "noodle.db" $ do
    runMigration migrateAll

createPoll name desc = do
  runSqlite "noodle.db" $ do
    id <- insert $ Poll name desc
    return id

getNewPollId id = unSqlBackendKey $ unPollKey id

allPolls = do
  runSqlite "noodle.db" $ do
    polls <- selectList ([] :: [Filter Poll]) [LimitTo 30, Desc PollId]
    return $ polls

getPollById id = do
  runSqlite "noodle.db" $ do
    selectList [PollId ==. (toSqlKey (read id))] [LimitTo 1]

pollNames = map (\i -> ((getPollId i), (pollName (entityVal i))))

getPollId x = unSqlBackendKey $ unPollKey $ entityKey x

pollValues i = ((getPollId i), (pollName (entityVal i)), (pollDesc (entityVal i)))

optionsValues = map (\o -> ((getOptionId o),
  (optionName (entityVal o)), (optionDesc (entityVal o))))

optionIds opts = map (\o -> getOptionId o) opts

getOptionId x = unSqlBackendKey $ unOptionKey $ entityKey x

getOptionsByPollId id = do
  runSqlite "noodle.db" $ do
    selectList [OptionPollId ==. (toSqlKey (read id))] []

createOption pId name desc = do
  runSqlite "noodle.db" $ do
    insert $ Option (toSqlKey (read pId)) name desc

getVotersByOptionIds ids = do
  mapM (\ oId -> do
    voters <- getVotersByOptionId oId
    return (oId, voters)) ids

voterValues = map (\(oId, voters) -> (oId, (map voterName voters)))

voterName vote = (voteVoter (entityVal vote))

getVotersByOptionId oId =
  runSqlite "noodle.db" $ do
    selectList [VoteOptionId ==. (toSqlKey oId)] []

voteForOptions name opts c_opt_ids = do
  mapM_ (\id -> runSqlite "noodle.db" $ do
    deleteWhere [VoteOptionId ==. (toSqlKey id), VoteVoter ==. name]
    ) opts
  mapM_ (\id -> runSqlite "noodle.db" $ do
    insert $ Vote id name
    ) choosen_ids
  where choosen_ids = map (\x -> (toSqlKey (read x))) c_opt_ids
