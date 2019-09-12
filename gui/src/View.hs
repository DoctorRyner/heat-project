module View where

import           Miso
import qualified Style.Global
import           Types
import           Utils
import qualified View.About
import qualified View.Content
import qualified View.Header
import qualified View.ObogrevKrovli
import qualified View.ObogrevPloshadi
import qualified View.ObogrevTruboprovoda

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
                    , onClick $ Batch [PopOr, SwitchMenu, changeRoute x.routePath model.uri]
                    ] [label_ [class_ "menuMes"] [text x.label]]
                ) menu
            ]
        else ""
    , div_ [] [View.Header.render model]
    , curRoute
    ]
  where
    curRoute = case uriToRouteString model.uri of
        ""                     -> View.Content.render model
        "about"                -> View.ObogrevKrovli.render "about" model
        "planning"             -> View.ObogrevKrovli.render "planning" model
        "montage"              -> View.ObogrevKrovli.render "montage" model
        "individ-proj"         -> View.ObogrevKrovli.render "individ-proj" model
        "obogrev-krovli"       -> View.ObogrevKrovli.render "obogrev-krovli" model
        "obogrev-truboprovoda" -> View.ObogrevKrovli.render "obogrev-truboprovoda" model
        "obogrev-ploshadi"     -> View.ObogrevKrovli.render "obogrev-ploshadi" model
        _                      -> "404 page"
