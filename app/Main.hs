module Main (main) where

import Lib
import System.Environment (getEnv)
import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack)
import Domain.Bot (botStartup)

-- main :: IO ()
-- main = do
--   testLib
--   someFunc


main :: IO ()
main = do
  -- envStr <- getEnv "CONN_STRING"
  -- let connStr = encodeUtf8 (pack envStr)
  -- _ <- runStdoutLoggingT . withPostgresqlPool connStr 1 . runSqlPool $ doMigration
  runApp botStartup