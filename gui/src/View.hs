module View where

import           Miso
import qualified Style.Global
import           Types
import           Utils

removeStartAndEndSlashesFromPath :: String -> String
removeStartAndEndSlashesFromPath str = drop 1 $ if take 1 reversedStr == "/" then reverse $ drop 1 reversedStr else str 
  where
    reversedStr = reverse str

view :: Model -> View Event
view model = case removeStartAndEndSlashesFromPath $ uriPath model.uri of
    "/"              -> root
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