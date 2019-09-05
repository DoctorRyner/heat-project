module View where

import           Miso
import qualified Style.Global
import           Types
import           Utils

view :: Model -> View Event
view model = div_ []
    [ maybeStyle model.files.normalizeCss
    , maybeStyle . Just $ Style.Global.css
    , h1_ [] [ "Hello, World!" ]
    , h1_ [] [ "Hello, World!" ]
    , button_ [ onClick JSTest ] [ "TEST" ]
    , text $ case model.files.normalizeCss of
        Just str -> str
        _        -> ""
    ]
