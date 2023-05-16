{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Model
  ( UserId,
    Username,
    MessageId,
    User (..),
    Message (..),
    MessageError (..),
    TranslateError (..),
    findBestWord,
    isRomanWord,
    cleanWord,
    CleanText (..),
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Internal.Fusion as TS (stream)
import qualified Data.Text.Internal.Fusion.Common as TS
import Data.Time (UTCTime)
import GHC.Exts (sortWith)

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

newtype MessageError = UserDoesNotExist UserId
  deriving (Show, Eq)

data TranslateError = WordNotFound
  deriving (Eq)

instance Show TranslateError where
  show WordNotFound = "Слово не найдено"

findBestWord :: [(Int, Text, Text)] -> Either TranslateError (Text, Text)
findBestWord [] = Left WordNotFound
findBestWord [(_, wordRom, worsRus)] = Right (wordRom, worsRus)
findBestWord wrds =
  Right
    . (\(_, wRom, wRus) -> (wRom, wRus))
    . head
    . sortWith (\(id_, _, _) -> id_)
    $ wrds

newtype CleanText = CleanText {getCleanText :: Text} -- lowered and cleaned from \ş -> ș and \ţ -> ț

isRomanWord :: CleanText -> Bool
isRomanWord (CleanText word) = any (`textElem` word) literalsRomana
  where
    literalsRomana :: [Char]
    literalsRomana = "ăâîșț" -- CleanText is lowered and cleaned from \ş -> ș and \ţ -> ț
    textElem :: Char -> Text -> Bool
    textElem c t = TS.any (== c) (TS.stream t)

cleanWord :: Text -> CleanText
cleanWord =
  CleanText
    . T.map (\c -> if c == 'ş' then 'ș' else c)
    . T.map (\c -> if c == 'ţ' then 'ț' else c)
    . T.toLower
