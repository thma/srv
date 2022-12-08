{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}

module Main where

import           Control.Concurrent.Async
import qualified Data.Yaml                      as Yaml
import           GHC.Generics                   (Generic)
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS

data WarpHandler = Http | Https deriving (Show, Eq, Generic, Yaml.FromJSON, Yaml.ToJSON)

data SrvConfig = SrvConfig
  { handlers   :: ![WarpHandler],
    httpPort   :: !Int,
    httpsPort  :: !Int,
    pathToCert :: !FilePath,
    pathToKey  :: !FilePath,
    dir        :: !FilePath
  }
  deriving (Show, Eq, Generic, Yaml.FromJSON, Yaml.ToJSON)

defaultConfig :: SrvConfig
defaultConfig =
  SrvConfig
    { handlers = [Http, Https],
      httpPort = 8080,
      httpsPort = 443,
      pathToCert = "certificate.pem",
      pathToKey = "key.pem",
      dir = "."
    }

main :: IO ()
main = do
  putStrLn "starting up srv..."
  let config = defaultConfig
  Yaml.encodeFile "config.yaml" config

  let tls = tlsSettings (pathToCert config) (pathToKey config)
      app = staticApp (defaultFileServerSettings (dir config))
      https = runTLS tls (setPort (httpsPort config) defaultSettings) app
      http = run (httpPort config) app
  forConcurrently_ (handlers config) $ \case
    Http  -> http
    Https -> https
