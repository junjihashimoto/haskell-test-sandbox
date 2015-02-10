{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}
{-#LANGUAGE OverloadedStrings#-}

import Test.Hspec
import Text.Shakespeare.Text

import Network.Wai
import Network.Wai.Test

import Yesod
import Data.IORef.Lifted
import Data.Typeable (Typeable)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8,decodeUtf8)

import Test.Sandbox.Compose.Type
import Test.Sandbox.Compose.Core
import Test.Sandbox.Compose.Server
import Test.Cabal.Path

main = do
  appref <- newIORef $ error "do not eval this"
  hspec $ do
    describe "Server Test" $ do
      it "read conf" $ do
        (app,_port) <- runServer' "sample/test-sandbox-compose.yml"
        writeIORef appref app
        return ()
      it "up" $ runner appref $ do
        res <- request defaultRequest { pathInfo = ["up"] }
        liftIO $ print res
        assertStatus 200 res
        assertBody "OK\n" res
      it "destroy" $ runner appref $ do
        res <- request defaultRequest { pathInfo = ["destory"] }
        assertStatus 200 res
        assertBody "OK\n" res
  
runner :: IORef App -> Session () -> IO ()
runner appref f = do
  app <- readIORef appref
  toWaiApp app >>= runSession f
