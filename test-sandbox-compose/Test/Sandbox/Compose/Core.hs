{-#LANGUAGE TemplateHaskell#-}

module Test.Sandbox.Compose.Core where

--import Data.Monoid
import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Test.Sandbox.Compose.Type
import Test.Sandbox.Compose.Template
import Test.Sandbox 
import Test.Sandbox.Internals
import qualified Data.Yaml as Y
import System.Process
import System.Exit
import Network.Socket (PortNumber(..))

getList :: (Service -> [a]) -> Services -> [(ServiceName,a)]
getList func services =
  concat
  $ map (\(k,v) -> map ((,) k) $ func v)
  $ M.toList services

getPortList :: Services -> [(ServiceName, PortName)]
getPortList = getList $ fromMaybe [] . sPorts
getTempList :: Services -> [(ServiceName, TempFileName)]
getTempList = getList $ fromMaybe [] . sTempFiles
getDirList :: Services -> [(ServiceName, DirName)]
getDirList = getList $ fromMaybe [] . sDirs
getConfList :: Services -> [(ServiceName, (ConfName, ConfContent))]
getConfList = getList $ (M.toList . fromMaybe M.empty . sConfs)

setupServices :: Services -> Sandbox (Maybe Services)
setupServices services = do
  ports <- forM (map (\(s,p) -> (s++"_port_"++p)) $ getPortList services) $ \portname ->   do
    portnum <- getPort portname
    return (portname,show portnum) :: Sandbox (String,String)
  temps <- forM (map (\(s,p) -> s++"_temp_"++p) $ getTempList services) $ \tempname -> do
    filename <- setFile tempname []
    return (tempname,show filename) :: Sandbox (String,String)
  dirs <- forM (map (\(s,p) -> s++"_dir_"++p) $ getDirList services) $ \dirname -> do
    dir <- getDataDir
    return (dirname,dir++"/"++dirname) :: Sandbox (String,String)
  let params = ports ++ temps ++ dirs
  mPreService  <- updateServices params (Just services)

  case mPreService of
    Nothing -> return Nothing
    Just preService -> do 
      confs <- forM (getConfList preService) $ \(s,(cn,_)) ->  do
        let tempname = s++"_conf_"++cn
        filename <- setFile tempname ""
        return (tempname,filename) :: Sandbox (String,String)
      mService  <-  updateServices confs mPreService
      case mService of
        Nothing -> return Nothing
        Just service ->  do
          forM_ (getConfList service) $ \(s,(cn,cc)) ->  do
            let tempname = s++"_conf_"++cn
            filename <- getFile tempname
            liftIO $ writeFile filename cc
          return mService
  where
    updateServices _ Nothing = return Nothing
    updateServices params (Just service) = do
      service' <- liftIO $ applyTemplate params $ B.unpack $ Y.encode service
      return $ Y.decode $ B.pack service'

runServices :: Services -> Sandbox ()
runServices services = do
  forM_ (M.toList services) $ \(k,_) -> do
    runService k services

runService :: ServiceName -> Services -> Sandbox ()
runService serviceName services = do
  dir <- getDataDir
  let k = serviceName
  let v = services M.! serviceName
  void $ register k (sCmd v) (sArgs v) (def{psWait = Just 3,psCapture=Just (CaptureBothWithFile (dir ++ "/" ++ k ++ "_out.txt") (dir ++ "/" ++ k ++ "_err.txt"))})
  case sBeforeScript v of
    Just script -> do
      r@(c,_,_) <- liftIO $ readProcessWithExitCode "bash" ["-c",script] []
      when (c /= ExitSuccess) $ do
        error $ "BeforeScript Error:\n" ++ show r
    Nothing -> return ()
  void $ start k
  case sAfterScript v of
    Just script -> do
      r@(c,_,_) <- liftIO $ readProcessWithExitCode "bash" ["-c",script] []
      when (c /= ExitSuccess) $ do
        error $ "AfterScript Error:\n" ++ show r
    Nothing -> return ()

killService :: ServiceName -> Sandbox ()
killService k = stop k


killServices :: Sandbox ()
killServices = do
  stopAll
  cleanUpProcesses
