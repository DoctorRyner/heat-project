module View where

import           Miso
import qualified Style.Global
import           Types
import           Utils
import qualified View.Header

view :: Model -> View Event
view model = div_ []
    [ maybeStyle model.files.normalizeCss
    , maybeStyle . Just $ Style.Global.css
    , View.Header.render model
    , curRoute
    ]
  where 
    curRoute = case uriToRouteString model.uri of
        ""               -> ""
        "about"          -> "about page"
        "about/company"  -> "about company page"
        _                -> "404 page"
    