{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Adapter.PostgreSQL.Adapter
  ( getUserById,
    createUser,
    insertMsg,
    translateWord,
  )
where

import Adapter.PostgreSQL.Common
import Data.Text (Text, toLower, unpack)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple
import Domain.Model
  ( Message (..),
    MessageError (..),
    TranslateError (..),
    User (..),
    UserId,
    Username,
  )
import qualified Domain.Model as M
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
    Just _ -> do
      result :: [(Int, UTCTime)] <- withConn $ \conn -> query conn qryStr (uId, text)
      case result of
        [(messageId, sent)] -> pure $ Right Message {messageId, uId, text, sent}
        _ -> throwString $ "Should not happen: cannot create user in users table in DB - username = " ++ show uId ++ " text = " ++ show text
  where
    qryStr = "insert into messages (user_id, text) values (?,?) returning id, sent"

translateWord :: PG r m => Text -> m (Either TranslateError (Text, Text))
translateWord wordUnclean = do
  let word = M.cleanWord wordUnclean
  let qryStr = if M.isRomanWord word then qryStrRom else qryStrEng
  result :: [(Int, Text, Text)] <- withConn $ \conn -> query conn qryStr (Only $ M.getCleanText word)
  pure $ M.findBestWord result
  where
    qryStrEng =
      "SELECT \
      \  min_id, \
      \  word_rom, \
      \  word_rus \
      \  from \
      \      ( \
      \    SELECT  \
      \      min(t.idTranslation) as min_id, \
      \      word_rom.word_rom as word_rom \
      \    from \
      \      word_rom  \
      \      join translations t on word_rom.word_eng = ? and t.idWord = word_rom.id \
      \      join word_rus rus on t.idTranslation = rus.id \
      \    group by word_rom.word_rom \
      \    ) as sq join word_rus w on  w.id = sq.min_id;"

    qryStrRom =
      "SELECT \
      \  min_id, \
      \  word_rom, \
      \  word_rus \
      \  from \
      \      ( \
      \    SELECT  \
      \      min(t.idTranslation) as min_id, \
      \      word_rom.word_rom as word_rom \
      \    from \
      \      word_rom  \
      \      join translations t on word_rom.word_rom = ? and t.idWord = word_rom.id \
      \      join word_rus rus on t.idTranslation = rus.id \
      \    group by word_rom.word_rom \
      \    ) as sq join word_rus w on  w.id = sq.min_id;"
