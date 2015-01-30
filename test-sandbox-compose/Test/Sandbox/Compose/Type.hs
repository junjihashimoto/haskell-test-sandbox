
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE StandaloneDeriving#-}

module Test.Sandbox.Compose.Type where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.Word
import qualified Data.Map as M
import qualified Data.Text as T
import Test.Sandbox.Internals
import Network
import Network.Socket hiding (ServiceName)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.IORef
import System.Process hiding (env, waitForProcess)
import System.Posix.Types
import System.Exit
import System.IO

type ServiceName = String
type PortName = String
type TempFileName = String
type DirName = String
type ConfName = String
type ConfContent = String

type Services = M.Map ServiceName Service
type PortList = [(ServiceName,PortName)]
type TempList = [(ServiceName,TempFileName)]
type DirList =  [(ServiceName,DirName)]
type ConfList = M.Map (ServiceName,(ConfName,ConfContent)) Service

data Service = Service {
  sCmd :: FilePath
, sArgs :: [String]
, sConfs :: Maybe (M.Map ConfName ConfContent)
, sDirs :: Maybe [DirName]
, sTempFiles :: Maybe [TempFileName]
, sPorts :: Maybe [PortName]
, sBeforeScript :: Maybe String
, sAfterScript :: Maybe String
} deriving (Show,Read,Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1.map toLower, constructorTagModifier = map toLower} ''Service)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 2.map toLower, constructorTagModifier = map toLower} ''SandboxState)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 2.map toLower, constructorTagModifier = map toLower} ''SandboxedProcess)
$(deriveJSON defaultOptions ''Capture)
$(deriveJSON defaultOptions ''SandboxedProcessInstance)
$(deriveJSON defaultOptions ''CPid)
$(deriveJSON defaultOptions ''ExitCode)

instance ToJSON ByteString where
  toJSON = toJSON . T.pack . B.unpack
instance FromJSON ByteString where
  parseJSON (String str) = pure $ B.pack $ T.unpack $ str

instance ToJSON PortNumber where
  toJSON (PortNum port) = String $ T.pack $ show port
instance FromJSON PortNumber where
  parseJSON (String str) =
    case (reads (T.unpack str)::[(Word16,String)]) of
      [(port,_)] -> pure $ PortNum port
      _ -> mzero
  parseJSON _ = mzero

instance ToJSON Handle where
  toJSON = toJSON . show
instance FromJSON Handle where
  parseJSON _ = pure $ stderr

instance ToJSON ProcessHandle where
  toJSON _ = toJSON $ show "ProcessHandle"
instance FromJSON ProcessHandle where
  parseJSON _ = mzero

data App = App {
  appServices :: IORef Services
, appState :: IORef SandboxState
}

