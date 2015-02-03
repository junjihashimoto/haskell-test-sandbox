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
import Control.Concurrent
import System.Exit
import System.Directory
import System.Process
import System.Posix.Process
import System.Posix


instance Yesod App where
  errorHandler e = liftIO (print e) >> defaultErrorHandler e

getUpAllR :: HandlerT App IO RepPlain
getUpAllR = do
  (App serv sand) <- getYesod
  result <- liftIO $ flip runSandbox sand $ do
    runServices =<< readIORef serv
  case result of
    Right _ -> return $ RepPlain "OK\n"
    Left err -> return $ RepPlain $ toContent $ err

getUpR :: ServiceName -> HandlerT App IO RepPlain
getUpR serviceName = do
  (App serv sand) <- getYesod
  result <- liftIO $ flip runSandbox sand $ do
    runService serviceName =<< readIORef serv
  case result of
    Right _ -> return $ RepPlain "OK\n"
    Left err -> return $ RepPlain $ toContent $ err


getStatusAllR :: HandlerT App IO RepPlain
getStatusAllR = do
  (App serv sand) <- getYesod
  sand' <- liftIO $ readIORef sand
  return $ RepPlain $ toContent $ Y.encode $ sand' {ssAvailablePorts=[]}
  
getStatusR :: ServiceName -> HandlerT App IO RepPlain
getStatusR serviceName = do
  (App serv sand) <- getYesod
  sand' <- liftIO $ readIORef sand
  return $ RepPlain $ toContent $ Y.encode $ sand' {ssAvailablePorts=[]}
  
getConfR :: HandlerT App IO RepPlain
getConfR = do
  (App serv sand) <- getYesod
  sand' <- liftIO $ readIORef serv
  return $ RepPlain $ toContent $ Y.encode $ sand'
  
 :: HandlerT App IO RepPlain
getKillAllR = do
  (App _ sand) <- getYesod
  result <- liftIO $ flip runSandbox sand $ do
    killServices
  case result of
    Right _ -> return  $ RepPlain "OK\n"
    Left err -> return $ RepPlain $ toContent $ err

getKillR :: ServiceName -> HandlerT App IO RepPlain
getKillR serviceName = do
  (App _ sand) <- getYesod
  result <- liftIO $ flip runSandbox sand $ do
    killService serviceName
  case result of
    Right _ -> return  $ RepPlain "OK\n"
    Left err -> return $ RepPlain $ toContent $ err


getDestroyR :: HandlerT App IO RepPlain
getDestroyR = do
  (App _ sand) <- getYesod
  result <- liftIO $ flip runSandbox sand $ do
    killServices
  liftIO $ forkIO $ do
    threadDelay (1*1000*1000)
    id <- getProcessID
    signalProcess sigTERM id
  return  $ RepPlain "OK\n"


getLogsAllR :: HandlerT App IO Value
getLogsAllR = error "not implemented"
  
getLogsR :: ServiceName -> HandlerT App IO Value
getLogsR serviceName = error "not implemented"
  
mkYesod "App" [parseRoutes|
/up                  UpAllR     GET
/up/#ServiceName     UpR        GET
/status              StatusAllR GET
/status/#ServiceName StatusR    GET
/conf                ConfR      GET
/kill                KillAllR   GET
/kill/#ServiceName   KillR      GET
/logs                LogsAllR   GET
/logs/#ServiceName   LogsR      GET
/destroy             DestroyR   GET
|]

main :: IO ()
main = do
  cdir <- getCurrentDirectory
  state <- newSandboxState "compose" (cdir ++ "/.sandbox")
  eServ <- Y.decodeFileEither "test-sandbox-compose.yml" :: IO (Either Y.ParseException Services)
  case eServ of
    Left err -> do
      print err
      exitWith $ ExitFailure 1
    Right serv -> do
      serv' <- runSandbox' state $ do
        mserv <- setupServices serv
        case mserv of
          Just s -> return s
          Nothing ->  liftIO $ do
            print "setupServices is failed."           
            exitWith $ ExitFailure 1
      services <- newIORef serv'
      toWaiApp (App services state) >>= run 3000
