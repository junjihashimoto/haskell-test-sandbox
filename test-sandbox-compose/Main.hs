{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}

import Test.Sandbox.Compose
import Options.Applicative

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Yesod.Core
import Data.Typeable (Typeable)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Data.Aeson
import Data.IORef.Lifted
import Test.Sandbox hiding (run)
import Test.Sandbox.Internals
import qualified Data.Yaml as Y
import qualified Data.Map as M

instance Yesod App where
  errorHandler e = liftIO (print e) >> defaultErrorHandler e

getUpAllR :: HandlerT App IO Value
getUpAllR = do
  (App serv sand) <- getYesod
  result <- liftIO $ flip runSandbox sand $ do
    runServices =<< readIORef serv
  case result of
    Right _ -> return $ String "OK"
    Left err -> return $ String $ T.pack $ show err

getUpR :: ServiceName -> HandlerT App IO Value
getUpR serviceName = do
  (App serv sand) <- getYesod
  result <- liftIO $ flip runSandbox sand $ do
    runService serviceName =<< readIORef serv
  case result of
    Right _ -> return $ String "OK"
    Left err -> return $ String $ T.pack $ show err


getStatusAllR :: HandlerT App IO Value
getStatusAllR = do
  (App serv sand) <- getYesod
  sand' <- liftIO $ readIORef sand
  return $ String $ T.pack $ BC.unpack $ Y.encode $ sand'
  
getConfR :: HandlerT App IO Value
getConfR = do
  (App serv sand) <- getYesod
  sand' <- liftIO $ readIORef serv
  return $ String $ T.pack $ BC.unpack $ Y.encode $ sand'
  
getKillAllR :: HandlerT App IO Value
getKillAllR = do
  (App _ sand) <- getYesod
  result <- liftIO $ flip runSandbox sand $ do
    killServices
  case result of
    Right _ -> return $ String "OK"
    Left err -> return $ String $ T.pack $ show err

getLogsAllR :: HandlerT App IO Value
getLogsAllR = error "not implemented"
  
getLogsR :: HandlerT App IO Value
getLogsR = error "not implemented"
  
mkYesod "App" [parseRoutes|
/up                  UpAllR     GET
/up/#ServiceName     UpR        GET
/status              StatusAllR GET
/status/#ServiceName StatusR    GET
/conf                ConfR      GET
/kill                KillAllR   GET
/kill/#ServiceName   KillR      GET
/log                 LogsAllR   GET
/log/#ServiceName    LogsR      GET
|]

main :: IO ()
main = do
  state <- newSandboxState "compose" ".sandbox"
  eServ <- Y.decodeFileEither "test-sandbox-compose.yml" :: IO (Either Y.ParseException Services)
  print $ eServ
  let (Right serv) = eServ
  print $ serv
  B.putStr $ Y.encode $ serv
  serv' <- runSandbox' state $ setupServices serv
  B.putStr $ Y.encode $ serv'
  print $ show serv'
  services <- newIORef serv'
  toWaiApp (App services state) >>= run 3000
