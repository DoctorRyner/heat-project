module View where

import           Miso
import qualified Style.Global
import           Types
import           Utils

view :: Model -> View Event
view model = div_ []
    [ link_ [ rel_ "stylesheet", type_ "text/css", href_ "static/css/normalize.css" ]
    , maybeStyle . Just $ Style.Global.css
    , h1_ [] [ "Hello, World!" ]
    , h1_ [] [ "Hello, World!" ]
    ]
