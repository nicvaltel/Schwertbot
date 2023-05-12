{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Lib
  ( runBot,
  )
where

import Adapter.PostgreSQL.Adapter qualified as PG
import Adapter.PostgreSQL.Common qualified as PG
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT))
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text, pack)
import Debug.Trace (traceShow)
import Domain.Bot qualified as B
import Domain.Model qualified as M
import Telegram.Bot.Simple (BotM, Eff, reply, toReplyMessage, (<#))

newtype App a = App {runApp :: IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadThrow)

instance M.MessengerDB App where
  getUserById = undefined
  createUser = undefined
  insertMsg = undefined

runBot :: IO ()
runBot = do
  Right pgCfg <- PG.readDBConfig "db/database.env"
  PG.withAppState pgCfg $ \pool ->
    B.botStartup_ (PG.getToken pgCfg) (handleAction pool)

getUserById_ :: PG.AppState -> M.UserId -> IO (Maybe M.User)
getUserById_ pool uId = runReaderT (PG.getUserById uId) pool

insertMsg_ :: PG.AppState -> M.UserId -> Text -> IO (Either M.MessageError M.Message)
insertMsg_ pool uId txt = runReaderT (PG.insertMsg uId txt) pool

createUser_ :: PG.AppState -> M.UserId -> M.Username -> IO M.User
createUser_ pool uId uName = runReaderT (PG.createUser uId uName) pool

handleAction :: PG.AppState -> M.Action -> M.ChatModel -> Eff M.Action M.ChatModel
handleAction pool action model = traceShow action $
  case action of
    M.NoAction -> pure model
    M.RecordMsg usrId mayUsrname msgId txt -> do
      let usrname = fromMaybe (pack $ "user_" <> show usrId) mayUsrname
      model <# do
        maybeUser :: Maybe M.User <- liftIO $ getUserById_ pool usrId
        when (isNothing maybeUser) $ liftIO $ createUser_ pool usrId usrname >> pure ()
        _ <- liftIO $ insertMsg_ pool usrId txt
        case maybeUser of
          Just user -> replyString "И снова здравствуйте. Сохраняю сообщение..."
          Nothing -> replyString "Здравтсвуйте. Приятно познакомиться, я бот-швертбот. Записываю ваше сообщение..."
        replyString "Готово"
        pure M.NoAction
  where
    replyString :: String -> BotM ()
    replyString = reply . toReplyMessage . pack
