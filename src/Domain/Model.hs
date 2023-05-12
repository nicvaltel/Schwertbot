{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Domain.Model where

import Control.Monad.Cont (MonadIO)
import Data.Text (Text)
import Data.Time (UTCTime)
import Telegram.Bot.Simple (Eff)

type UserId = Int

type Username = Text

type MessageId = Int

data User = User
  { userId :: UserId,
    username :: Username,
    created :: UTCTime
  }
  deriving (Show, Eq)

data Message = Message
  { messageId :: MessageId,
    uId :: Int,
    text :: Text,
    sent :: UTCTime
  }
  deriving (Show, Eq)

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

newtype MessageError = UserDoesNotExist UserId
  deriving (Show, Eq)

class Monad m => MessengerDB m where
  getUserById :: Int -> m (Maybe User)
  createUser :: UserId -> Username -> m User
  insertMsg :: UserId -> Text -> m (Either MessageError Message)

class (Monad m, MonadIO m) => BotClass m where
  botStartup :: String -> (Action -> ChatModel -> Eff Action ChatModel) -> m ()
