module Main where

import Network.Wai.Handler.Warp 
import Network.Wai.Application.Static
import Network.Wai.Handler.WarpTLS

main :: IO ()
main = do 
   let tls = defaultTlsSettings  -- tlsSettings "pathToCert" "pathToKey"
       app = (staticApp (defaultFileServerSettings "."))
   --runTLS tls (setPort 443 defaultSettings) app
   run 80 app