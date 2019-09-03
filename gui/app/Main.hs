module Main where

import           App                              (app)
import           Language.Javascript.JSaddle.Warp (run)
import           Miso                             (startApp)

main :: IO ()
main = run 8000 $ startApp app
