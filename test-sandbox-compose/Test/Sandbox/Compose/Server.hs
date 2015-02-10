{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Test.Sandbox.Compose.Server where

import Test.Sandbox.Compose.Type
import Test.Sandbox.Compose.Core

import Network.Wai.Handler.Warp (run)
import Yesod.Core
import qualified Data.Text as T
import Data.IORef.Lifted
import Test.Sandbox hiding (run)
import Test.Sandbox.Internals hiding (Port)
import qualified Data.Yaml as Y
import qualified Data.Map as M
import Control.Concurrent
import System.Exit
import System.Directory
import System.Posix.Process
import System.Posix hiding (Kill)
import Shelly hiding (command,run,FilePath)
import Data.Monoid

default (T.Text)

instance Yesod App where
  errorHandler e = liftIO (appendFile ".sandbox/log" (show e)) >> defaultErrorHandler e

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
  (App _serv sand) <- getYesod
  sand' <- liftIO $ readIORef sand
  return $ RepPlain $ toContent $ Y.encode $ sand' {ssAvailablePorts=[]}
  
getStatusR :: ServiceName -> HandlerT App IO RepPlain
getStatusR serviceName = do
  (App _serv sand) <- getYesod
  sand' <- liftIO $ readIORef sand
  return $ RepPlain $ toContent $ Y.encode $ M.lookup serviceName $ ssProcesses sand'
  
getConfR :: HandlerT App IO RepPlain
getConfR = do
  (App serv _sand) <- getYesod
  sand' <- liftIO $ readIORef serv
  return $ RepPlain $ toContent $ Y.encode $ sand'
  
getKillAllR :: HandlerT App IO RepPlain
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
  _ <- liftIO $ flip runSandbox sand $ do
    killServices
  _ <- liftIO $ forkIO $ do
    threadDelay (1*1000*1000)
    shelly $ do
      rm ".sandbox/port" 
    getProcessID >>= signalProcess sigTERM
  return  $ RepPlain "OK\n"


getLogsDaemonR :: HandlerT App IO RepPlain
getLogsDaemonR = do
  out <- liftIO $ readFile (".sandbox/" <> ".server" <> "_out.txt")
  err <- liftIO $ readFile (".sandbox/" <> ".server" <> "_err.txt")
  return $ RepPlain $ toContent $  out <> err
  
getLogsR :: ServiceName -> HandlerT App IO RepPlain
getLogsR serviceName = do
  out <- liftIO $ readFile (".sandbox/" <> serviceName <> "_out.txt")
  err <- liftIO $ readFile (".sandbox/" <> serviceName <> "_err.txt")
  return $ RepPlain $ toContent $  out <> err
  
  
mkYesod "App" [parseRoutes|
/up                  UpAllR     GET
/up/#ServiceName     UpR        GET
/status              StatusAllR GET
/status/#ServiceName StatusR    GET
/conf                ConfR      GET
/kill                KillAllR   GET
/kill/#ServiceName   KillR      GET
/logs                LogsDaemonR   GET
/logs/#ServiceName   LogsR      GET
/destroy             DestroyR   GET
|]


runServer' :: FilePath -> IO (App,Int)
runServer' conf = do
  cdir <- getCurrentDirectory
  let dir=(cdir <> "/.sandbox")
  shelly $ do
    unlessM (test_e ".sandbox") $
      mkdir ".sandbox"
  state <- newSandboxState "compose" dir
  port <- runSandbox' state $ do
    getPort "test-sandbox-compose"
  writeFile (dir <> "/port") $ show port
  eServ <- Y.decodeFileEither conf :: IO (Either Y.ParseException Services)
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
      return (App services state,port)

runServer :: FilePath -> Bool -> IO ()
runServer conf enbBackGround = do
  (app,port) <- runServer' conf
  let prog :: IO ()
      prog = toWaiApp app >>= run port
  backGround enbBackGround prog

backGround :: Bool -> IO () -> IO ()
backGround exitP prog = do
  _ <- forkProcess p
  when exitP $ do
    exitImmediately ExitSuccess
  where
    p  = do
      _ <- createSession
      _ <- forkProcess p'
      exitImmediately ExitSuccess
    p' = do
      switchDescriptors
      prog
    switchDescriptors :: IO ()
    switchDescriptors = do
      null' <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
      out <- openFd ".sandbox/.server_out.txt" ReadWrite (Just 0o644) defaultFileFlags
      err <- openFd ".sandbox/.server_err.txt" ReadWrite (Just 0o644) defaultFileFlags
      let sendTo' fd' fd = closeFd fd >> dupTo fd' fd
      _ <- sendTo' null' stdInput
      _ <- sendTo' out stdOutput
      _ <- sendTo' err stdError
      return ()

