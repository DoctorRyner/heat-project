module Main where

import           App        (app)
import           Miso       (startApp)
import           TestServer (run)

main :: IO ()
main = run 8000 $ startApp app
