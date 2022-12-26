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
import           Network.Wai.Handler.WarpTLS    (runTLS, tlsSettings, TLSSettings)
import           System.Directory               (doesFileExist)
import           DemoCertificate                (demoTLSSettings)


import           System.Process           (createProcess, shell, ProcessHandle)
import           System.Info              (os)
import           GHC.IO.Handle.Types (Handle)


-- | a data type representing Http or Https warp handlers
data WarpHandler = HTTP | HTTPS deriving (Show, Eq, Generic, Yaml.FromJSON, Yaml.ToJSON)

-- | a tuple (WarpHandler, IP port)
type Handler = (WarpHandler, Int)

-- | configuration settings that specify the behaviour of the server
data SrvConfig = SrvConfig
  { handlers     :: ![Handler],
    pathToCert   :: !(Maybe FilePath),
    pathToKey    :: !(Maybe FilePath),
    documentRoot :: !FilePath
  }
  deriving (Show, Eq, Generic, Yaml.FromJSON, Yaml.ToJSON)

-- | create a configuration with default settings
defaultConfig :: SrvConfig
defaultConfig =
  SrvConfig
    { handlers     = [(HTTP, 8080), (HTTPS, 8443)],
      pathToCert   = Nothing, -- Just "certificate.pem",
      pathToKey    = Nothing, -- Just "key.pem",
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
  launchSiteInBrowser HTTP port
  run port $ staticAppFrom config

-- create an HTTPS handler action based on config settings
httpsHandler :: Handler -> SrvConfig -> IO ()
httpsHandler (_, port) config = do
  putStrLn $ "Starting HTTPS server on port " <> show port
  launchSiteInBrowser HTTPS port
  runTLS
    (getTlsSettings (pathToCert config) (pathToKey config))
    (setPort port defaultSettings)
    (staticAppFrom config)

-- | create a TLS settings object from file path settings. If no file paths are
-- provided, use the demo in-memory certificate
getTlsSettings :: Maybe FilePath -> Maybe FilePath -> TLSSettings
getTlsSettings (Just cert) (Just key) = tlsSettings cert key
getTlsSettings _ _ = demoTLSSettings

-- | derive all handler IO actions from the configuration
getAllHandlers :: SrvConfig -> [IO ()]
getAllHandlers config =
  map (handlerToAction config) (handlers config)
  where
    handlerToAction :: SrvConfig -> Handler -> IO ()
    handlerToAction cfg handler = case handler of
      (HTTP, _)  -> httpHandler handler cfg
      (HTTPS, _) -> httpsHandler handler cfg

configFile :: FilePath
configFile = "srv.yaml"

-- | start all handlers configured in config.yaml
main :: IO ()
main = do
  putStrLn "starting up srv..."
  configIsPresent <- doesFileExist configFile
  config <-
    if configIsPresent
      then do
        putStrLn $ "reading " ++ configFile ++ "..."
        Yaml.decodeFileThrow configFile
      else do
        putStrLn $ configFile ++ " not found, generating file with default config"
        Yaml.encodeFile configFile defaultConfig
        return defaultConfig
  -- run all handlers defined in config
  mapConcurrently_ id (getAllHandlers config)


-- | launch the site in the default browser. 
launchSiteInBrowser :: WarpHandler -> Int -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
launchSiteInBrowser handler port =
  case os of
    "mingw32" -> createProcess  (shell $ "start " ++ url)
    "darwin"  -> createProcess  (shell $ "open " ++ url)
    _         -> createProcess  (shell $ "xdg-open " ++ url)
  where 
    url = case handler of
      HTTP  -> "http://localhost:" ++ show port
      HTTPS -> "https://localhost:" ++ show port
