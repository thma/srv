module Main where

import           Control.Concurrent.Async
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS

data WarpHandler = Http | Https | Both deriving (Show, Eq)

data SrvConfig = SrvConfig {
    handler :: WarpHandler
  , httpPort :: Int
  , httpsPort :: Int
  , pathToCert :: FilePath
  , pathToKey :: FilePath
  , dir :: FilePath
} deriving (Show, Eq)

defaultConfig :: SrvConfig
defaultConfig = SrvConfig {
    handler = Both
  , httpPort = 8080
  , httpsPort = 443
  , pathToCert = "certificate.pem"
  , pathToKey  = "key.pem"
  , dir = "."
}

main :: IO ()
main = do
  putStrLn "starting up srv..."
  let config = defaultConfig
  print config

  let tls   = tlsSettings (pathToCert config) (pathToKey config)
      app   = staticApp (defaultFileServerSettings (dir config))
      https = runTLS tls (setPort (httpsPort config) defaultSettings) app
      http  = run (httpPort config) app
  case handler config of
    Http  -> http
    Https -> https
    Both  -> concurrently_ http https

