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
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Map as M
import Data.Text.Lazy as T (unpack, pack)
import Data.Monoid (mconcat)

import qualified Noodle.Views.Index
import qualified Noodle.Views.Show
import qualified Noodle.Views.New
import qualified Noodle.Views.Edit
import qualified Noodle.Views.EditName

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Poll
    name String
    desc String
    createdAt UTCTime
    deriving Show
  Cant
    pollId PollId
    name String
    createdAt UTCTime
    UniqueCant pollId name
    deriving Show
  Option
    pollId PollId
    name String
    desc String
    createdAt UTCTime
    deriving Show
  Vote
    optionId OptionId
    voter String
    createdAt UTCTime
    UniqueVote optionId voter
    deriving Show
  |]

blaze = S.html . renderHtml
main :: IO ()
main = do
  initDb
  scottySite

scottySite = S.scotty 3000 $ do
    S.get "/noodle.css" $ S.file "noodle.css"
    S.get "/polls" $ do
      polls <- liftIO getPolls
      blaze $ Noodle.Views.Index.render $ pollNames polls
    S.get "/" $ S.redirect "/polls"
    S.get "/polls/new" $ blaze $ Noodle.Views.New.render []
    S.post "/options/delete" $ do
      id <- S.param "id" :: S.ActionM String
      all_params <- S.params
      let choosen_opt_ids = foldl (\ acc (key, value) -> if key == "option_id"
          then T.unpack value:acc
          else acc) [] all_params
      deleteOptions choosen_opt_ids
      S.redirect $ T.pack $ "/polls/" ++ id ++ "/edit"
    S.get "/polls/:id/delete" $ do
      id <- S.param "id"
      deletePoll id
      S.redirect "/polls"
    S.post "/polls/:id/vote" $ do
      (id, name, options) <- getPollFromParam
      all_params <- S.params
      let choosen_opt_ids = foldl (\ acc (key, value) -> if key == "option_id"
          then T.unpack value:acc
          else acc) [] all_params
      case (validVote name) of
        False -> (showAction id
          ["Vote needs the name who votes and must not contain '?'."] "")
        otherwise -> do
          doVoting name (optionIds options) choosen_opt_ids id
          S.redirect $ T.pack $ "/polls/" ++ id
    S.get "/polls/:id/edit" $ do
      id <- S.param "id"
      poll <- liftIO $ getPollById id
      options <- liftIO $ getOptionsByPollId id
      blaze $ Noodle.Views.Edit.render
        (pollValues $ head poll) (optionsValues options) []
    S.get "/polls/:id/edit_name" $ do
      id <- S.param "id"
      poll <- liftIO $ getPollById id
      blaze $ Noodle.Views.EditName.render (pollValues $ head poll) []
    S.get "/polls/:id" $ do
      id <- S.param "id"
      showAction id [] ""
    S.get "/polls/:id/vote/:name/edit" $ do
      id <- S.param "id" :: S.ActionM String
      name <- S.param "name" :: S.ActionM String
      showAction id [] name
    S.get "/polls/:id/vote/:name/delete" $ do
      (id, name, options) <- getPollFromParam
      deleteVote id name (optionIds options)
      S.redirect $ T.pack $ "/polls/" ++ id
    S.post "/polls/:id/update" $ do
      id <- S.param "id"
      name <- S.param "name"
      desc <- S.param "desc"
      poll <- liftIO $ getPollById id
      case name of
        "" -> blaze $ Noodle.Views.EditName.render (pollValues $ head poll)
                [ "Poll needs a name" ]
        otherwise -> do
          updatePoll id name desc
          S.redirect $ T.pack $ "/polls/" ++ id
    S.post "/polls/" $ do
      name <- S.param "name"
      desc <- S.param "desc"
      case name of
        "" -> blaze $ Noodle.Views.New.render [ "A Poll needs a name" ]
        otherwise -> do
          newId <- liftIO $ createPoll name desc
          S.redirect $ T.pack $ "/polls/" ++ show (getNewPollId newId) ++ "/edit"
    S.post "/options/" $ do
      name <- S.param "name" :: S.ActionM String
      desc <- S.param "desc" :: S.ActionM String
      pId <- S.param "id":: S.ActionM String
      poll <- liftIO $ getPollById pId
      options <- liftIO $ getOptionsByPollId pId
      case name of
        "" -> blaze $ Noodle.Views.Edit.render (pollValues $ head poll)
                   (optionsValues options) [ "Option needs a name" ]
        otherwise -> do
          createOption pId name desc
          S.redirect $ T.pack $ "/polls/" ++ pId ++ "/edit"

getPollFromParam = do
  id <- S.param "id" :: S.ActionM String
  name <- S.param "name" :: S.ActionM String
  options <- liftIO $ getOptionsByPollId id
  return (id, name, options)


showAction id errors editVoter = do
  poll <- liftIO $ getPollById id
  options <- liftIO $ getOptionsByPollId id
  voters <- liftIO $ getVotesByOptionIds (optionIds options)
  cants <- liftIO $ getCantsByPollId id
  blaze $ (Noodle.Views.Show.render (pollValues $ head poll)
    (optionsValues options) (getVoteNames voters) (cantNames cants) errors
      editVoter)

initDb = runSqlite "noodle.db" $ runMigration migrateAll

createPoll name desc = do
  now <- liftIO $ getCurrentTime
  runSqlite "noodle.db" $ insert $ Poll name desc now

createCant id name opt_ids = do
  now <- liftIO $ getCurrentTime
  mapM_ (\i -> runSqlite "noodle.db" $
      deleteWhere [VoteOptionId ==. toSqlKey i, VoteVoter ==. name]) opt_ids
  runSqlite "noodle.db" $ do
    deleteWhere [CantPollId ==. pollId, CantName ==. name]
    insert $ Cant pollId name now
  return ()
  where pollId = toSqlKey (read id)

deletePoll id = runSqlite "noodle.db" $
  deleteWhere [PollId ==. pollId]
  where pollId = toSqlKey (read id)

updatePoll id name desc = do
  now <- liftIO $ getCurrentTime
  runSqlite "noodle.db" $ replace pollId $ Poll name desc now
  where pollId = toSqlKey (read id)

getNewPollId id = unSqlBackendKey $ unPollKey id

getPolls = runSqlite "noodle.db" $
  selectList ([] :: [Filter Poll]) [LimitTo 30, Desc PollId]

getPollById id = runSqlite "noodle.db" $
  selectList [PollId ==. toSqlKey (read id)] [LimitTo 1]

pollNames = map (\i -> (getPollId i, pollName $ entityVal i))

cantNames = map $ cantName . entityVal

getPollId x = unSqlBackendKey $ unPollKey $ entityKey x

pollValues i = (getPollId i, pollName $ entityVal i, pollDesc $ entityVal i)

optionsValues = map (\o -> (getOptionId o, optionName $ entityVal o,
  optionDesc $ entityVal o))

optionIds = map getOptionId

getOptionId x = unSqlBackendKey $ unOptionKey $ entityKey x

getOptionsByPollId id = runSqlite "noodle.db" $
  selectList [OptionPollId ==. toSqlKey (read id)] []

getCantsByPollId id = runSqlite "noodle.db" $
    selectList [CantPollId ==. toSqlKey (read id)] []

createOption pId name desc = do
  now <- liftIO $ getCurrentTime
  runSqlite "noodle.db" $ insert $
    Option (toSqlKey (read pId)) name desc now

getVotesByOptionIds ids = do
  votes <- mapM getVotersByOptionId ids
  return $ foldl (foldl (flip (:))) [] votes

getVoteNames = foldl voteNameMap M.empty

voteNameMap acc vote =
  case M.lookup vName acc of
    Just ids -> M.insert vName (vOptId:ids) acc
    Nothing -> M.insert vName [vOptId] acc
  where vName = voterName vote
        vOptId = unSqlBackendKey $ unOptionKey $ voterOptId vote

voterValues = map (\(oId, voters) -> (oId, map voterName voters))

voterName vote = voteVoter (entityVal vote)
voterOptId vote = voteOptionId (entityVal vote)

getVotersByOptionId oId = runSqlite "noodle.db" $
  selectList [VoteOptionId ==. toSqlKey oId] [Asc VoteCreatedAt]

deleteOptions ids = runSqlite "noodle.db" $
    mapM_ (\id -> runSqlite "noodle.db" $ do
      deleteWhere [VoteOptionId ==. id]
      deleteWhere [OptionId ==. id]
      ) choosen_ids
  where choosen_ids = map (toSqlKey . read ) ids

voteForOptions name opts c_opt_ids id = do
  now <- liftIO $ getCurrentTime
  mapM_ (\i -> runSqlite "noodle.db" $
    deleteWhere [VoteOptionId ==. toSqlKey i, VoteVoter ==. name]) opts
  runSqlite "noodle.db" $ deleteWhere [CantPollId ==. pollId, CantName ==. name]
  mapM_ (\i -> runSqlite "noodle.db" $ insert $ Vote i name now) choosen_ids
  where choosen_ids = map (toSqlKey . read) c_opt_ids
        pollId = toSqlKey (read id)

doVoting name opt_ids choosen_opt_ids id =
  case length choosen_opt_ids of
    0 -> createCant id name opt_ids
    otherwise -> voteForOptions name opt_ids choosen_opt_ids id

deleteVote id name opts = do
  mapM_ (\i -> runSqlite "noodle.db" $
    deleteWhere [VoteOptionId ==. toSqlKey i, VoteVoter ==. name]) opts
  runSqlite "noodle.db" $ deleteWhere [CantPollId ==. pollId, CantName ==. name]
  where pollId = toSqlKey (read id)

validVote name = validLength && validChars
  where validLength = (length name) > 0
        validChars = foldl (\ acc c -> c /= '?' && acc) True name
