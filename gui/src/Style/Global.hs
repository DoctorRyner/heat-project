module Style.Global where

import           Clay
import           Miso.String

css :: MisoString
css = Miso.String.ms . render $ do
    body ? pure ()
    pure ()
