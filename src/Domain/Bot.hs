{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Bot (botStartup_) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe
import Data.Text
import Domain.Model qualified as M
import Telegram.Bot.API as Telegram
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

botStartup_ :: (MonadIO m) => String -> (M.Action -> M.ChatModel -> Eff M.Action M.ChatModel) -> m ()
botStartup_ tokenStr handleAction = do
  let token = Token . pack $ tokenStr
  env <- liftIO $ defaultTelegramClientEnv token
  liftIO $ startBot_ (conversationBot updateChatId (incexpBotApp handleAction)) env

emptyChatModel :: M.ChatModel
emptyChatModel = M.ChatModel M.InitSate

incexpBotApp :: (M.Action -> M.ChatModel -> Eff M.Action M.ChatModel) -> BotApp M.ChatModel M.Action
incexpBotApp handleAction = BotApp {botInitialModel = emptyChatModel, botAction = flip handleUpdate, botHandler = handleAction, botJobs = []}

handleUpdate :: M.ChatModel -> Update -> Maybe M.Action
handleUpdate model update =
  let msg = fromJust $ updateMessage update
      usr = fromJust $ messageFrom msg
      Telegram.UserId usrId = Telegram.userId usr
      Telegram.MessageId msgId = Telegram.messageMessageId msg
      usrIdInt = fromIntegral usrId :: Int
      msgIdInt = fromIntegral msgId :: Int
      usrName = Telegram.userUsername usr
      parser = M.RecordMsg usrIdInt usrName msgIdInt <$> plainText
   in parseUpdate parser update
