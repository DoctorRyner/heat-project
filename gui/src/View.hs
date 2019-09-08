module View where

import           Miso
import qualified Style.Global
import           Types
import           Utils
import qualified View.Header
import qualified View.Content

view :: Model -> View Event
view model = div_ []
    [ maybeStyle model.files.normalizeCss
    , maybeStyle . Just $ Style.Global.css
    , View.Header.render model
    , curRoute
    ]
  where 
    curRoute = case uriToRouteString model.uri of
        ""               -> View.Content.render model
        "about"          -> "about page"
        "about/company"  -> "about company page"
        _                -> "404 page"
    