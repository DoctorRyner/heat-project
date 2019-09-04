module Utils where

import           Control.Monad.IO.Class      (liftIO)
import           Language.Javascript.JSaddle hiding ((<#))
import           Miso
import           Miso.String
import           Types

newtype Console a = Console { log :: Show a => a -> JSM () }

console :: a => Console
console = Console
    { log = consoleLog =<< val
    }

withJS :: model -> JSM event -> Effect event model
withJS = (<#)

withIO :: model -> IO event -> Effect event model
withIO model = (model <#) . liftIO

maybeStyle :: Maybe MisoString -> View Event
maybeStyle = \case
    Just cssText -> nodeHtml "style" [] [ text cssText ]
    Nothing      -> ""

