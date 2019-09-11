module View where

import           Miso
import qualified Style.Global
import           Types
import           Utils
import qualified View.Header
import qualified View.Content
import qualified View.About

view :: Model -> View Event
view model = div_ []
    [ maybeStyle model.files.normalizeCss
    , maybeStyle . Just $ Style.Global.css model
    , if model.device == Mobile && model.shouldShowMenu 
        then div_ [] 
            [ div_ [class_ "menuMob"]
                $ map
                (\x -> div_
                    [ class_ "menu-item-mob"
                    , onClick $ changeRoute x.routePath model.uri
                    ] [label_ [class_ "menuMes"] [text x.label]]
                ) menu
            ]
        else ""
    , div_ [] [View.Header.render model]
    , curRoute
    ]
  where
    curRoute = case uriToRouteString model.uri of
        ""               -> 
--        View.About.render model
            View.Content.render model
        "about"          -> 
       
            View.About.render model
        "planning"       -> text $ mshow model.uri 
        "montage"        -> text $ mshow model.uri 
        "individ-proj"   -> text $ mshow model.uri 
        "about/company"  -> "about company page"
        _                -> "404 page"
