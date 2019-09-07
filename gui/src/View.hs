module View where

import           Miso
import qualified Style.Global
import           Types
import           Utils

rPath :: String -> String
rPath str = drop 1 $ str <> if take 1 (reverse str) /= "/" then "/" else ""

view :: Model -> View Event
view model = case rPath $ uriPath model.uri of
    "/"              -> root
    ""               -> root
    "about/"         -> button_ [ onClick $ ChangeURI $ changer "" ] [ "WRIIIIIIIIII" ]
    "about/company/" -> label_ [ onClick $ ChangeURI $ changer "" ] [ "HAAA" ]
    _                -> "404 page"
  where
    root = div_ []
        [ maybeStyle model.files.normalizeCss
        , maybeStyle . Just $ Style.Global.css
        , label_ [ onClick $ changeRoute "about/company" model.uri ] [ text $ mshow model.uri ]
        ]