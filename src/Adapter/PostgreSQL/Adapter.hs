{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Adapter.PostgreSQL.Adapter where

import Adapter.PostgreSQL.Common
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple
import Domain.Model
  ( Message (..),
    MessageError (UserDoesNotExist),
    User (..),
    UserId,
    Username,
  )
import UnliftIO (throwString)

getUserById :: PG r m => UserId -> m (Maybe User)
getUserById uid = do
  result :: [(Int, Text, UTCTime)] <- withConn $ \conn -> query conn qryStr (Only uid)
  case result of
    [] -> pure Nothing
    [(userId, username, created)] -> pure $ Just User {userId, username, created}
    _ -> throwString $ "Should not happen: several userId's in users table in DB - id = " ++ show uid
  where
    qryStr = "select user_id, username, created from users where user_id = ?"

createUser :: PG r m => UserId -> Username -> m User
createUser userId username = do
  result :: [Only UTCTime] <- withConn $ \conn -> query conn qryStr (userId, username)
  case result of
    [Only created] -> pure User {userId, username, created}
    _ -> throwString $ "Should not happen: cannot create user in users table in DB - username = " ++ show username
  where
    qryStr = "insert into users (user_id, username) values (?, ?) returning created"

insertMsg :: PG r m => UserId -> Text -> m (Either MessageError Message)
insertMsg uId text = do
  mayUser <- getUserById uId
  case mayUser of
    Nothing -> pure (Left $ UserDoesNotExist uId)
    Just user_ -> do
      result :: [(Int, UTCTime)] <- withConn $ \conn -> query conn qryStr (uId, text)
      case result of
        [(messageId, sent)] -> pure $ Right Message {messageId, uId, text, sent}
        _ -> throwString $ "Should not happen: cannot create user in users table in DB - username = " ++ show uId ++ " text = " ++ show text
  where
    qryStr = "insert into messages (user_id, text) values (?,?) returning id, sent"
