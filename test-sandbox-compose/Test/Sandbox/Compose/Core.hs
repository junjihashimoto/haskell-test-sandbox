{-#LANGUAGE TemplateHaskell#-}

module Test.Sandbox.Compose.Core where

--import Data.Monoid
import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Test.Sandbox.Compose.Type
import Test.Sandbox.Compose.Template
import Test.Sandbox 
import Test.Sandbox.Internals
import qualified Data.Yaml as Y

getList :: (Service -> [a]) -> Services -> [(ServiceName,a)]
getList func services =
  concat
  $ map (\(k,v) -> map ((,) k) $ func v)
  $ M.toList services

getPortList :: Services -> [(ServiceName, PortName)]
getPortList = getList sPorts
getTempList :: Services -> [(ServiceName, TempFileName)]
getTempList = getList sTempFiles
getDirList :: Services -> [(ServiceName, DirName)]
getDirList = getList sDirs
getConfList :: Services -> [(ServiceName, (ConfName, ConfContent))]
getConfList = getList (M.toList . sConfs)

setupServices :: Services -> Sandbox Services
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
--   confs <- forM (getConfList services) $ \(s,(cn,cc)) ->  do
--     let tempname = s++"_conf_"++cn
-- --    filename <- setFile tempname cc
--     return (tempname,"{{"++tempname++"}}") :: Sandbox (String,String)
  let params = ports ++ temps ++ dirs
--  liftIO $ print $ ports ++ temps ++ dirs ++ confs
  v' <- liftIO $ applyTemplate params $ B.unpack $ Y.encode services
  let (Just v'') = Y.decode $ B.pack v' :: Maybe Services
  confs <- forM (getConfList v'') $ \(s,(cn,cc)) ->  do
    let tempname = s++"_conf_"++cn
    filename <- setFile tempname cc
    return (tempname,filename) :: Sandbox (String,String)
  liftIO $ print $ ports ++ temps ++ dirs ++ confs
  v' <- liftIO $ applyTemplate confs $ B.unpack $ Y.encode v''
  let (Just v'') = Y.decode $ B.pack v'
  return $ v''

runServices :: Services -> Sandbox ()
runServices services = do
  dir <- getDataDir
  forM_ (M.toList services) $ \(k,v) -> do
    void $ register k (sCmd v) (sArgs v) (def{psWait = Just 3,psCapture=Just (CaptureBothWithFile (dir ++ "/" ++ k ++ "_out.txt") (dir ++ "/" ++ k ++ "_err.txt"))})
    void $ start k

runService :: ServiceName -> Services -> Sandbox ()
runService serviceName services = do
  dir <- getDataDir
  let k = serviceName
  let v = services M.! serviceName
  void $ register k (sCmd v) (sArgs v) (def{psWait = Just 3,psCapture=Just (CaptureBothWithFile (dir ++ "/" ++ k ++ "_out.txt") (dir ++ "/" ++ k ++ "_err.txt"))})
  void $ start k

killServices :: Sandbox ()
killServices = do
  stopAll
  cleanUpProcesses
