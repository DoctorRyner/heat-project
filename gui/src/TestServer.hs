module TestServer where

import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Static
import           Network.WebSockets                     (defaultConnectionOptions)

import           Language.Javascript.JSaddle.Run        (syncPoint)
import           Language.Javascript.JSaddle.Types      (JSM)
import           Language.Javascript.JSaddle.WebSockets

runWithStaticFiles :: Int -> JSM () -> IO ()
runWithStaticFiles port f = runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
    jsaddleOr defaultConnectionOptions (f >> syncPoint) staticAppServing
  where
    staticAppServing = staticPolicy (addBase "./" <|> addSlash) jsaddleApp
