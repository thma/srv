module Main where

import           Control.Concurrent.Async
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS

main :: IO ()
main = do
  let tls   = defaultTlsSettings -- tlsSettings "pathToCert" "pathToKey"
      app   = staticApp (defaultFileServerSettings ".")
      https = runTLS tls (setPort 443 defaultSettings) app
      http  = run 80 app
  _ <- concurrently http https
  return ()
