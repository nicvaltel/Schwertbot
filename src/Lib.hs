{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Lib
  ( 
    AppState,
    Messenger(..),
    App(..),
    runApp,
    someFunc,
    testLib,
  )
where

import Adapter.PostgreSQL.Common qualified as PG
import Adapter.PostgreSQL.Adapter qualified as PG
import Control.Monad.Catch
import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT (runReaderT))
import Katip
import System.IO (stdout)
import Domain.Model (Messenger(..))
import Adapter.PostgreSQL.Common (getToken)


type AppState = (PG.AppState, String)

newtype App a = App {unApp :: ReaderT AppState (KatipContextT IO) a}
  deriving (Functor, Applicative, Monad, MonadReader AppState, MonadIO, MonadFail, KatipContext, Katip, MonadThrow)

instance Messenger App where
  getUserById = PG.getUserById
  createUser = PG.createUser
  insertMsg = PG.insertMsg

run :: LogEnv -> AppState -> App a -> IO a
run le state app =
  runKatipContextT le () mempty $
    runReaderT (unApp app) state

someFunc :: IO ()
someFunc = putStrLn "someFunc"

withKatip :: (LogEnv -> IO a) -> IO a
withKatip app = do
  bracket createLogEnv closeScribes app
  where
    createLogEnv = do
      logEnv <- initLogEnv "Quorum" "dev"
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

runApp :: App a -> IO a
runApp app = withKatip $ \le -> do
  Right pgCfg <- PG.readDBConfig "db/database.env"
  PG.withAppState pgCfg $ \pgState ->
    run le (pgState, getToken pgCfg) app

testLib :: IO ()
testLib = withKatip $ \le -> do
  Right pgCfg <- PG.readDBConfig "db/database.env"
  PG.withAppState pgCfg $ \pgState ->
    run le (pgState, getToken pgCfg) testAction1

testAction1 :: App ()
testAction1 = do
  user <- createUser 777 "TestUser1"
  let uId = user.userId
  mayUser <- getUserById uId
  liftIO $ putStrLn $ "User: " ++ show mayUser
  message <- insertMsg uId "Test Message"
  liftIO $ putStrLn $ "Message: " ++ show message
  liftIO $ print "Done!"
