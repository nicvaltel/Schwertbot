{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Lib
  ( runBot,
  )
where

import Adapter.PostgreSQL.Adapter qualified as PG
import Adapter.PostgreSQL.Common qualified as PG
import Control.Monad (when)
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT))
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text, pack)
import Debug.Trace (traceShow)
import Domain.Bot qualified as B
import Domain.Model qualified as M
import Telegram.Bot.Simple (BotM, Eff, reply, toReplyMessage, (<#))

runBot :: IO ()
runBot = do
  Right pgCfg <- PG.readDBConfig "db/database.env"
  PG.withAppState pgCfg $ \pool ->
    B.botStartup (PG.getToken pgCfg) (handleTranslate pool)

getUserById_ :: PG.AppState -> M.UserId -> IO (Maybe M.User)
getUserById_ pool uId = runReaderT (PG.getUserById uId) pool

insertMsg_ :: PG.AppState -> M.UserId -> Text -> IO (Either M.MessageError M.Message)
insertMsg_ pool uId txt = runReaderT (PG.insertMsg uId txt) pool

createUser_ :: PG.AppState -> M.UserId -> M.Username -> IO M.User
createUser_ pool uId uName = runReaderT (PG.createUser uId uName) pool

translateWord_ :: PG.AppState -> Text -> IO (Either M.TranslateError (Text, Text))
translateWord_ pool word = runReaderT (PG.translateWord word) pool

-- handleAction :: PG.AppState -> B.Action -> B.ChatModel -> Eff B.Action B.ChatModel
-- handleAction pool action model = traceShow action $
--   case action of
--     B.NoAction -> pure model
--     B.RecordMsg usrId mayUsrname _ txt -> do
--       let usrname = fromMaybe (pack $ "user_" <> show usrId) mayUsrname
--       model <# do
--         maybeUser :: Maybe M.User <- liftIO $ getUserById_ pool usrId
--         when (isNothing maybeUser) $ liftIO $ createUser_ pool usrId usrname >> pure ()
--         _ <- liftIO $ insertMsg_ pool usrId txt
--         case maybeUser of
--           Just _ -> replyString "И снова здравствуйте. Сохраняю сообщение..."
--           Nothing -> replyString "Здравтсвуйте. Приятно познакомиться, я бот-швертбот. Записываю ваше сообщение..."
--         replyString "Готово"
--         pure B.NoAction
--   where
--     replyString :: String -> BotM ()
--     replyString = reply . toReplyMessage . pack

handleTranslate :: PG.AppState -> B.Action -> B.ChatModel -> Eff B.Action B.ChatModel
handleTranslate pool action model = traceShow action $
  case action of
    B.NoAction -> pure model
    B.RecordMsg usrId mayUsrname _ word -> do
      let usrname = fromMaybe (pack $ "user_" <> show usrId) mayUsrname
      model <# do
        maybeUser :: Maybe M.User <- liftIO $ getUserById_ pool usrId
        when (isNothing maybeUser) $ liftIO $ createUser_ pool usrId usrname >> pure ()
        _ <- liftIO $ insertMsg_ pool usrId word
        case maybeUser of
          Just _ -> do
            translation <- liftIO $ translateWord_ pool word
            case translation of
              Right (wRom, wRus) -> replyString $ wRom <> ": " <> wRus
              Left err -> replyString . pack $ show err
          Nothing -> replyString "Я бот-переводчик с румынского на русский язык. Напишите слово, чтобы я перевел его."
        pure B.NoAction
  where
    replyString :: Text -> BotM ()
    replyString = reply . toReplyMessage
