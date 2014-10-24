{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import Test.Hspec
import Test.Sandbox
import Test.Sandbox.Internals (get)
import Text.Heredoc
import qualified Text.Hastache as H
import qualified Text.Hastache.Context as H
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Map as M
import System.Posix.Files
import System.Directory
import Data.IORef
import Control.Monad.Trans.Reader
import Control.Monad.Reader
import Text.Regex.Posix
import System.Process
import System.Posix.Process
import Data.Maybe
--import Data.String.Here

main = 
  hspec $ do
    describe "Basic Test" $ do
      it "interactive Test by sandbox" $ do
        sandbox "hogehoge" $ do
          start =<< register "sed_regex" "sed" [ "-u", "s/a/b/" ] def { psCapture = Just CaptureStdout }
          v <- interactWith "sed_regex" "a\n" 1
          liftIO $
            v `shouldBe` "b\n"
      it "interactive Test by withSandbox" $ do
        withSandbox $ \ref -> do
          val <- runSB ref $ do
            start =<< register "sed_regex" "sed" [ "-u", "s/a/b/" ] def { psCapture = Just CaptureStdout }
            interactWith "sed_regex" "a\n" 1
          val `shouldBe` "b\n"
    describe "create script" $ do
      it "setFile" $ do
        withSandbox $ \ref -> do
          let foo = "aaaa"
          let content = 
                  [str|#!/bin/env bash
                      |${foo}
                      |
                      |while true ; do echo hhh ; sleep 1;done
                      |]
          putStr content
          file <- runSB ref $ setFile "str1" content
          readFile file `shouldReturn`  content
    describe "create script" $ do
      it "setFile" $ do
        val <- newIORef $ error "do not eval this"
        withSandbox $ \ref -> runSB ref $ do 
          file <- setFile "str1"
                  [str|#!/usr/bin/env bash
                      |trap 1 2 3 15
                      |while true ; do echo hhh ; sleep 1;done
                      |]
          liftIO $ setExecuteMode file
          fil2 <- setFile' "str2"
                  [("file",file)]
                  [str|#!/usr/bin/env bash
                      |{{file}}&
                      |{{file}}&
                      |{{file}}&
                      |trap 1 2 3 15
                      |while true ; do echo hhh ; sleep 1;done
                      |]
          liftIO $ setExecuteMode fil2
          _ <- register "scr1" fil2 [] def
          startAll
          stat <- get
          liftIO $ writeIORef val stat
        True `shouldBe` True

setFile' filename keyValues template  = do
  str <- H.hastacheStr
         H.defaultConfig
         (H.encodeStr template)
         (H.mkStrContext context)
  setFile filename $ T.unpack $ TL.toStrict str
  where
    context str = (M.fromList (map (\(k,v) -> (k,H.MuVariable v)) keyValues)) M.! str

setExecuteMode file = do
  print $ file
  stat <- getFileStatus file
  setFileMode file (fileMode stat `unionFileModes` ownerExecuteMode)
  

data ProcessInfo = ProcessInfo {
   piPid  :: String
,  piStat :: String
,  piPpid :: String
,  piPgid :: String
} deriving (Show,Eq,Read)

getProcessInfo :: String -> Maybe ProcessInfo
getProcessInfo v =
  if v =~ pattern
    then
      case v =~ pattern of
        [[_str,pid,stat,ppid,pgid]] -> Just $ ProcessInfo pid stat ppid pgid
        _ -> Nothing
    else
      Nothing
  where
    pattern = "^([0-9]+) \\([^\\)]*\\) ([RSDZTW]) ([0-9]+) ([0-9]+) [0-9]+ .*"

getProcessInfos :: IO [ProcessInfo]
getProcessInfos = do
  dirs <- getDirectoryContents "/proc"
  let processes = filter ( =~ "[0-9]+") dirs
  stats <- forM processes $ \ps -> do
    file <- readFile $ "/proc/" ++ ps ++ "/stat"
    return $ getProcessInfo file
  return $ catMaybes stats 


-- setPgid :: IO ()
-- setPgid = do
--   pid <- getProcessID
--   pgid <- createProcessGroupFor pid
--   joinProcessGroup pgid


-- killGroup :: IO ()
-- killGroup = do
--   pid <- getProcessID
--   pgid <- getProcessGroupID
--   pis <- getProcessInfos
--   let killingList =  filter (\p -> piPgid p == show pgid && piPid p /= show pid) pis
--   forM_ killingList $ \ps ->
--     signalProcess sigKILL $ read $ piPid ps

