{-# LANGUAGE OverloadedStrings #-}
module Level07.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks)

import           Data.Bifunctor                     (first)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow,
                                                     Query (fromQuery), ToRow)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level07.AppM                      (App, Env (envDB), liftEither)

import           Level07.Types                     (Comment, CommentText,
                                                     DBFilePath (getDBFilePath),
                                                     Error (DBError),
                                                     FirstAppDB (FirstAppDB, dbConn),
                                                     Topic, fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: DBFilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open ( getDBFilePath fp )
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

getDBConn
  :: App Connection
getDBConn = asks $ dbConn . envDB
  -- error "getDBConn not implemented"

runDB
  :: (a -> Either Error b)
  -> (Connection -> IO a)
  -> App b
runDB f g = do
  conn <- getDBConn
  r <- liftIO $ first DBError <$> Sql.runDBAction (g conn)
  liftEither $ f =<< r
  -- error "runDB not re-implemented"

getComments
  :: Topic
  -> App [Comment]
getComments t =
  let q = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
      f conn = Sql.query conn q (Sql.Only . getTopic $ t)
  in
    runDB (traverse fromDBComment) f
  --error "Copy your completed 'getComments' and refactor to match the new type signature"

addCommentToTopic
  :: Topic
  -> CommentText
  -> App ()
addCommentToTopic t c =
  let q =
        -- Remember that the '?' are order dependent so if you get your input
        -- parameters in the wrong order, the types won't save you here. More on that
        -- sort of goodness later.
        "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
      f conn = do
        nowish <- getCurrentTime
        Sql.execute conn q (getTopic t, getCommentText c, nowish)
        in
  -- We use the execute function this time as we don't care about anything
  -- that is returned. The execute function will still return the number of rows
  -- affected by the query, which in our case should always be 1.
    runDB Right f
  -- error "Copy your completed 'appCommentToTopic' and refactor to match the new type signature"

getTopics
  :: App [Topic]
getTopics =
  let q = "SELECT DISTINCT topic FROM comments"
      f conn =  Sql.query_ conn q
  in
    runDB (traverse ( mkTopic . Sql.fromOnly )) f
  -- error "Copy your completed 'getTopics' and refactor to match the new type signature"

deleteTopic
  :: Topic
  -> App ()
deleteTopic t =
  let q = "DELETE FROM comments WHERE topic = ?"
      f conn = Sql.execute conn q (Sql.Only . getTopic $ t)
  in
    runDB Right f
  -- error "Copy your completed 'deleteTopic' and refactor to match the new type signature"

-- Go on to 'src/Level07/Core.hs' next.
