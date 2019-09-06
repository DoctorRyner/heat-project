module Utils where

import           Control.Monad.IO.Class      (liftIO)
import           Language.Javascript.JSaddle hiding ((<#), JSM)
import           Miso
import           Miso.String
import           Types

mshow :: Show a => a -> MisoString
mshow = ms . show

logJS :: MisoString -> JSM ()
logJS str = consoleLog =<< val (unpack str)

logJS' :: Show a => a -> JSM ()
logJS' str = consoleLog =<< val (show str)

withJS :: model -> JSM event -> Effect event model
withJS = (<#)

withIO :: model -> IO event -> Effect event model
withIO model = (model <#) . liftIO

maybeStyle :: Maybe MisoString -> View Event
maybeStyle = \case
    Just cssText -> nodeHtml "style" [] [ text cssText ]
    Nothing      -> ""

