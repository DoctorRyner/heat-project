module TestServer where

import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Static
import           Network.WebSockets                     (defaultConnectionOptions)

import           Language.Javascript.JSaddle.Run        (syncPoint)
import           Language.Javascript.JSaddle.Types      (JSM)
import           Language.Javascript.JSaddle.WebSockets


run :: Int -> JSM () -> IO ()
run port f = runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
    jsaddleOr defaultConnectionOptions (f >> syncPoint) staticAppServing
  where
    staticAppServing = staticPolicy (addBase "./" <|> addSlash) jsaddleApp

debug :: Int -> JSM () -> IO ()
debug port f = do
    debugWrapper $ \withRefresh registerContext ->
        runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
            jsaddleOr defaultConnectionOptions
                (registerContext >> f >> syncPoint)
                (staticPolicy (addBase "./" <|> addSlash) $ withRefresh $ jsaddleAppWithJs $ jsaddleJs True)
    putStrLn $ "<a href=\"http://localhost:" <> show port <> "\">run</a>"