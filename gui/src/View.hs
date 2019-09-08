module View where

import           Miso
import qualified Style.Global
import           Types
import           Utils

view :: Model -> View Event
view model = case uriToRouteString model.uri of
    ""               -> root
    -- Example routes (DON'T USE / IN END OF ROUTE LIKE IN "about/company/" THIS WILL NOT WORK)
    "about"          -> "about page"
    "about/company"  -> "about company page"
    _                -> "404 page"
  where
    root = div_ []
        [ maybeStyle model.files.normalizeCss
        , maybeStyle . Just $ Style.Global.css
        , label_ [ onClick $ changeRoute "about/company" model.uri ] [ text $ mshow model.uri ]
        ]