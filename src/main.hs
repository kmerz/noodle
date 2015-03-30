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
import qualified Data.Map as M
import Data.Text.Lazy as T (unpack, pack)
import Data.Monoid (mconcat)

import qualified Noodle.Views.Index
import qualified Noodle.Views.Show
import qualified Noodle.Views.New
import qualified Noodle.Views.Edit

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Poll
    name String
    desc String
    deriving Show
  Cant
    pollId PollId
    name String
    UniqueCant pollId name
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
      S.redirect "/polls"
    S.get "/polls/new" $ do
      blaze $ Noodle.Views.New.render []
    S.post "/options/delete" $ do
      id <- S.param "id" :: S.ActionM String
      all_params <- S.params
      let choosen_opt_ids = foldl (\ acc (key, value) -> if key == "option_id"
          then (T.unpack value):acc
          else acc) [] all_params
      deleteOptions choosen_opt_ids
      S.redirect $ T.pack $ "/polls/" ++ id ++ "/edit"
    S.post "/polls/:id/vote" $ do
      id <- S.param "id" :: S.ActionM String
      name <- S.param "name" :: S.ActionM String
      options <- liftIO $ getOptionsByPollId id
      all_params <- S.params
      let choosen_opt_ids = foldl (\ acc (key, value) -> if key == "option_id"
          then (T.unpack value):acc
          else acc) [] all_params
      case name of
        "" -> showAction id ["Vote needs the name who votes."] ""
        otherwise -> do
          doVoting name (optionIds options) choosen_opt_ids id
          S.redirect $ T.pack $ "/polls/" ++ id
    S.get "/polls/:id/edit" $ do
      id <- S.param "id"
      poll <- liftIO $ getPollById id
      options <- liftIO $ getOptionsByPollId id
      blaze $ Noodle.Views.Edit.render
        (pollValues $ head poll) (optionsValues options) []
    S.get "/polls/:id" $ do
      id <- S.param "id"
      showAction id [] ""
    S.get "/polls/:id/vote/:name/edit" $ do
      id <- S.param "id" :: S.ActionM String
      name <- S.param "name" :: S.ActionM String
      showAction id [] name
    S.get "/polls/:id/vote/:name/delete" $ do
      id <- S.param "id" :: S.ActionM String
      name <- S.param "name" :: S.ActionM String
      options <- liftIO $ getOptionsByPollId id
      deleteVote id name (optionIds options)
      S.redirect $ T.pack $ "/polls/" ++ id
    S.post "/polls/:id/update" $ do
      id <- S.param "id"
      name <- S.param "name"
      desc <- S.param "desc"
      poll <- liftIO $ getPollById id
      options <- liftIO $ getOptionsByPollId id
      case name of
        "" -> blaze $ Noodle.Views.Edit.render
                (pollValues $ head poll) (optionsValues options)
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
          S.redirect $ T.pack $ "/polls/" ++ (show (getNewPollId newId))
    S.post "/options/" $ do
      name <- S.param "name" :: S.ActionM String
      desc <- S.param "desc" :: S.ActionM String
      pId <- S.param "id":: S.ActionM String
      poll <- liftIO $ getPollById pId
      options <- liftIO $ getOptionsByPollId pId
      case name of
        "" -> do blaze $ Noodle.Views.Edit.render (pollValues $ head poll)
                   (optionsValues options) [ "Option needs a name" ]
        otherwise -> do
          createOption pId name desc
          S.redirect $ T.pack $ "/polls/" ++ pId ++ "/edit"

showAction id errors editVoter = do
  poll <- liftIO $ getPollById id
  options <- liftIO $ getOptionsByPollId id
  voters <- liftIO $ getVotesByOptionIds (optionIds options)
  cants <- liftIO $ getCantsByPollId id
  blaze $ Noodle.Views.Show.render (pollValues $ head poll)
    (optionsValues options) (getVoteNames voters) (cantNames cants) [] editVoter

initDb = do
  runSqlite "noodle.db" $ do
    runMigration migrateAll

createPoll name desc = do
  runSqlite "noodle.db" $ do
    id <- insert $ Poll name desc
    return id

createCant id name opt_ids = do
  mapM_ (\i -> do
    runSqlite "noodle.db" $ do
      deleteWhere [VoteOptionId ==. (toSqlKey i), VoteVoter ==. name]
    ) opt_ids
  runSqlite "noodle.db" $ do
    deleteWhere [CantPollId ==. pollId, CantName ==. name]
    insert $ Cant pollId name
  return ()
  where pollId = (toSqlKey (read id))

updatePoll id name desc = do
  runSqlite "noodle.db" $ do
    replace pollId $ Poll name desc
  where pollId = (toSqlKey (read id))

getNewPollId id = unSqlBackendKey $ unPollKey id

allPolls = do
  runSqlite "noodle.db" $ do
    polls <- selectList ([] :: [Filter Poll]) [LimitTo 30, Desc PollId]
    return $ polls

getPollById id = do
  runSqlite "noodle.db" $ do
    selectList [PollId ==. (toSqlKey (read id))] [LimitTo 1]

pollNames = map (\i -> ((getPollId i), (pollName (entityVal i))))

cantNames c = map (\i -> (cantName (entityVal i))) c

getPollId x = unSqlBackendKey $ unPollKey $ entityKey x

pollValues i = ((getPollId i), (pollName (entityVal i)), (pollDesc (entityVal i)))

optionsValues = map (\o -> ((getOptionId o),
  (optionName (entityVal o)), (optionDesc (entityVal o))))

optionIds opts = map (\o -> getOptionId o) opts

getOptionId x = unSqlBackendKey $ unOptionKey $ entityKey x

getOptionsByPollId id = do
  runSqlite "noodle.db" $ do
    selectList [OptionPollId ==. (toSqlKey (read id))] []

getCantsByPollId id = do
  runSqlite "noodle.db" $ do
    selectList [CantPollId ==. (toSqlKey (read id))] []

createOption pId name desc = do
  runSqlite "noodle.db" $ do
    insert $ Option (toSqlKey (read pId)) name desc

getVotesByOptionIds ids = do
  votes <- mapM (\ oId -> do
    voters <- getVotersByOptionId oId
    return (voters)) ids
  let flat_votes = foldl (\acc x -> foldl (\a y -> y:a) acc x) [] votes
  return flat_votes

getVoteNames votes = foldl voteNameMap M.empty votes

voteNameMap acc vote =
  case M.lookup vName acc of
    Just ids -> M.insert vName (vOptId:ids) acc
    Nothing -> M.insert vName (vOptId:[]) acc
  where vName = voterName vote
        vOptId = unSqlBackendKey $ unOptionKey $ voterOptId vote

voterValues = map (\(oId, voters) -> (oId, (map voterName voters)))

voterName vote = (voteVoter (entityVal vote))
voterOptId vote = (voteOptionId (entityVal vote))

getVotersByOptionId oId =
  runSqlite "noodle.db" $ do
    selectList [VoteOptionId ==. (toSqlKey oId)] [Asc VoteId]

deleteOptions ids = do
  runSqlite "noodle.db" $ do
    mapM_ (\id -> runSqlite "noodle.db" $ do
      deleteWhere [VoteOptionId ==. id]
      deleteWhere [OptionId ==. id]
      ) choosen_ids
  where choosen_ids = map (\x -> (toSqlKey (read x))) ids

voteForOptions name opts c_opt_ids id = do
  mapM_ (\i -> runSqlite "noodle.db" $ do
    deleteWhere [VoteOptionId ==. (toSqlKey i), VoteVoter ==. name]
    ) opts
  runSqlite "noodle.db" $ do
    deleteWhere [CantPollId ==. pollId, CantName ==. name]
  mapM_ (\i -> runSqlite "noodle.db" $ do
    insert $ Vote i name
    ) choosen_ids
  where choosen_ids = map (\x -> (toSqlKey (read x))) c_opt_ids
        pollId = (toSqlKey (read id))

doVoting name opt_ids choosen_opt_ids id = do
  case (length choosen_opt_ids) of
    0 -> createCant id name opt_ids
    otherwise -> voteForOptions name opt_ids choosen_opt_ids id

deleteVote id name opts = do
  mapM_ (\i -> runSqlite "noodle.db" $ do
    deleteWhere [VoteOptionId ==. (toSqlKey i), VoteVoter ==. name]
    ) opts
  runSqlite "noodle.db" $ do
    deleteWhere [CantPollId ==. pollId, CantName ==. name]
  where pollId = (toSqlKey (read id))
