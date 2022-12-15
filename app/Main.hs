{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Main where

import           Control.Concurrent.Async       (mapConcurrently_)
import qualified Data.Yaml                      as Yaml
import           GHC.Generics                   (Generic)
import           Network.Wai                    (Application)
import           Network.Wai.Application.Static (defaultFileServerSettings,
                                                 staticApp)
import           Network.Wai.Handler.Warp       (defaultSettings, run, setPort)
import           Network.Wai.Handler.WarpTLS    (runTLS, tlsSettings)
import           System.Directory               (doesFileExist)

data WarpHandler = HTTP | HTTPS deriving (Show, Eq, Generic, Yaml.FromJSON, Yaml.ToJSON)

-- | a tuple (WarpHandler, IP port)
type Handler = (WarpHandler, Int)

-- | configuration settings that specify the behaviour of the server
data SrvConfig = SrvConfig
  { handlers     :: ![Handler],
    pathToCert   :: !FilePath,
    pathToKey    :: !FilePath,
    documentRoot :: !FilePath
  }
  deriving (Show, Eq, Generic, Yaml.FromJSON, Yaml.ToJSON)

-- | create a configuration with default settings
defaultConfig :: SrvConfig
defaultConfig =
  SrvConfig
    { handlers     = [(HTTP, 8080), (HTTPS, 8443)],
      pathToCert   = "certificate.pem",
      pathToKey    = "key.pem",
      documentRoot = "."
    }

-- | create a static WAI Application from a configuration
staticAppFrom :: SrvConfig -> Application
staticAppFrom config =
  staticApp $
    defaultFileServerSettings (documentRoot config)

-- create an HTTP handler action based on config settings
httpHandler :: Handler -> SrvConfig -> IO ()
httpHandler (_, port) config = do
  putStrLn $ "Starting HTTP server on port " <> show port
  run port $ staticAppFrom config

-- create an HTTPS handler action based on config settings
httpsHandler :: Handler -> SrvConfig -> IO ()
httpsHandler (_, port) config = do
  putStrLn $ "Starting HTTPS server on port " <> show port
  runTLS
    (tlsSettings (pathToCert config) (pathToKey config))
    (setPort port defaultSettings)
    (staticAppFrom config)

-- | derive all handler IO actions from the configuration
getAllHandlers :: SrvConfig -> [IO ()]
getAllHandlers config =
  map (handlerToAction config) (handlers config)
  where
    handlerToAction :: SrvConfig -> Handler -> IO ()
    handlerToAction cfg handler = case handler of
      (HTTP, _)  -> httpHandler handler cfg
      (HTTPS, _) -> httpsHandler handler cfg

-- | start all handlers configured in config.yaml
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
  -- run all handlers defined in config
  mapConcurrently_ id (getAllHandlers config)
