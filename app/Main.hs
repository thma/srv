{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Concurrent.Async
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import System.Directory

data WarpHandler = Http | Https deriving (Show, Eq, Generic, Yaml.FromJSON, Yaml.ToJSON)

type Handler = (WarpHandler, Int)

data SrvConfig = SrvConfig
  { handlers :: ![Handler],
    pathToCert :: !FilePath,
    pathToKey :: !FilePath,
    documentRoot :: !FilePath
  }
  deriving (Show, Eq, Generic, Yaml.FromJSON, Yaml.ToJSON)

defaultConfig :: SrvConfig
defaultConfig =
  SrvConfig
    { handlers = [(Http, 8080), (Https, 8443)],
      pathToCert = "certificate.pem",
      pathToKey = "key.pem",
      documentRoot = "."
    }

staticAppFrom :: SrvConfig -> Application
staticAppFrom config =
  staticApp $
    defaultFileServerSettings (documentRoot config)

httpHandler :: Handler -> SrvConfig -> IO ()
httpHandler (_, port) config = do
  putStrLn $ "Starting HTTP server on port " <> show port
  run port $ staticAppFrom config

httpsHandler :: Handler -> SrvConfig -> IO ()
httpsHandler (_, port) config = do
  putStrLn $ "Starting HTTPS server on port " <> show port
  runTLS
    (tlsSettings (pathToCert config) (pathToKey config))
    (setPort port defaultSettings)
    (staticAppFrom config)

deriveActions :: SrvConfig -> [IO ()]
deriveActions config =
  map (handlerToAction config) (handlers config)
  where
    handlerToAction :: SrvConfig -> Handler -> IO ()
    handlerToAction cfg handler = case handler of
      (Http, _) -> httpHandler handler cfg
      (Https, _) -> httpsHandler handler cfg

main :: IO ()
main = do
  putStrLn "starting up srv..."
  configIsPresent <- doesFileExist "config.yaml"
  config <-
    if configIsPresent
      then do
        putStrLn "reading config.yaml..."
        Yaml.decodeFileThrow "config.yaml"
      else do
        putStrLn "config.yaml not found, generating file with default config"
        Yaml.encodeFile "config.yaml" defaultConfig
        return defaultConfig

  mapConcurrently_ id (deriveActions config)
