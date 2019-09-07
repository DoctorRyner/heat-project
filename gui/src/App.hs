module App where

import qualified Miso
import           Types  (Event (..), Model, defaultModel)
import           Update (update)
import           View   (view)
--import Network.Uri

app :: Miso.App Model Event
app = Miso.App
    { initialAction = Init
    , model         = defaultModel
    , update        = flip update
    , view          = view
    , events        = Miso.defaultEvents
    , subs          = [ Miso.uriSub HandleURI ]
    , mountPoint    = Nothing
    }

runApp :: Miso.JSM ()
runApp = Miso.startApp app