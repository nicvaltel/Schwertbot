{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Domain.Model where
import Data.Text (Text)
import Data.Time (UTCTime)

type UserId = Int
type Username = Text
type MessageId = Int

data User = User {
  userId :: UserId,
  username :: Username,
  created :: UTCTime
} deriving (Show, Eq)

data Message = Message {
  messageId :: MessageId,
  uId :: Int,
  text :: Text,
  sent :: UTCTime
} deriving (Show, Eq)

-- data RegistrationError = RegistrationErrorIdTaken
--   deriving (Show, Eq)

data MessageError = UserDoesNotExist UserId
  deriving (Show, Eq)

class Monad m => Messenger m where
  getUserById :: Int -> m (Maybe User)
  createUser :: UserId -> Username -> m User
  insertMsg :: UserId -> Text -> m (Either MessageError Message)