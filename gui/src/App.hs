module App where

import qualified Miso
import           Types  (Event (..), Model, defaultModel)
import           Update (update)
import           View   (view)

app :: Miso.App Model Event
app = Miso.App
    { initialAction = Init
    , model         = defaultModel
    , update        = flip update
    , view          = view
    , events        = Miso.defaultEvents
    , subs          = []
    , mountPoint    = Nothing
    }