{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Domain.Bot where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Maybe
import Data.Text
import Domain.Model qualified as M
import Domain.Model(Messenger(..))
import Lib (App)
-- import Lib qualified as Lib
import Adapter.PostgreSQL.Common qualified as PG
import Adapter.PostgreSQL.Adapter qualified as PG

import System.Environment (getEnv)
import Telegram.Bot.API as Telegram
import Telegram.Bot.Simple
import Telegram.Bot.Simple.Debug
import Telegram.Bot.Simple.UpdateParser
import Control.Monad.RWS (MonadReader)
import Control.Monad.Reader (asks)
import Control.Monad.RWS (ask)
import Debug.Trace (traceShow)

newtype BotM' a = BotM' {unBotM' :: BotM a}
  deriving (Functor, Applicative, Monad, MonadReader BotContext, MonadIO)

instance M.Messenger BotM' where
  getUserById = PG.getUserById
  createUser = PG.createUser
  insertMsg = PG.insertMsg

data ChatState
  = InitSate
  deriving (Show, Eq)

newtype ChatModel
  = ChatModel ChatState
  deriving (Show, Eq)

data Action
  = NoAction
  | RecordMsg Int (Maybe Text) Int Text
  deriving (Show, Read)

-- class Monad m => Bot m where
--   botStartup :: m ()

botStartup :: PG.PG r m => App ()
botStartup = do
  -- env_token <- liftIO $ getEnv "TOKEN"
  (_, env_token) <- ask
  let token = Token . pack $ env_token
  env <- liftIO $ defaultTelegramClientEnv token
  liftIO $ startBot_ (conversationBot updateChatId incexpBotApp) env

emptyChatModel :: ChatModel
emptyChatModel = ChatModel InitSate

incexpBotApp :: BotApp ChatModel Action
incexpBotApp = BotApp {botInitialModel = emptyChatModel, botAction = flip handleUpdate, botHandler = handleAction, botJobs = []}

handleUpdate :: ChatModel -> Update -> Maybe Action
handleUpdate model update =
  let msg = fromJust $ updateMessage update
      usr = fromJust $ messageFrom msg
      Telegram.UserId usrId = Telegram.userId usr
      Telegram.MessageId msgId = Telegram.messageMessageId msg
      usrIdInt = fromIntegral usrId :: Int
      msgIdInt = fromIntegral msgId :: Int
      usrName = Telegram.userUsername usr
      parser = RecordMsg usrIdInt usrName msgIdInt <$> plainText
   in parseUpdate parser update

-- (<#) :: GetAction a action => model -> BotM a -> Eff action model -- import Telegram.Bot.Simple.Eff
(<##) ::  GetAction a action => model -> BotM' a -> Eff action model
(<##) model botM' = model <# unBotM' botM'

handleAction :: Action -> ChatModel -> Eff Action ChatModel
handleAction action model =
  traceShow action $
  case action of
    NoAction -> traceShow "HERE1" $ pure model
    RecordMsg usrId mayUsrname msgId txt ->
      let usrname = fromMaybe (pack $ "user_" <> show usrId) mayUsrname
       in traceShow "HERE2" $ model <## do
            traceShow "HERE3" $ pure ()
            maybeUser :: Maybe M.User <- (getUserById usrId :: BotM' (Maybe M.User))
            traceShow "HERE4" $ pure ()
            case maybeUser of
              Just user -> do
                BotM' $ replyString "Hi again. Recording your msg..."
              Nothing -> do
                userKy <- M.createUser usrId usrname
                BotM' $ replyString "Hi. Nice to meet you, I'm a simple bot that records msgs. Recording your msg..."
            _ <- insertMsg usrId txt
            BotM' $ replyString "Done"
            pure NoAction

replyString :: String -> BotM ()
replyString = reply . toReplyMessage . pack
