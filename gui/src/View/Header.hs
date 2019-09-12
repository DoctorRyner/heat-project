module View.Header where

import Types
import Miso
import Utils

render :: Model -> View Event
render model = div_ [class_ "header"] $
    img_ [class_ "img", src_ "static/img/logo.png", onClick $ changeRoute "" model.uri] : arg
  where
    arg = case model.device of
        PC ->
            [ div_  [class_ "mcont"]
                [ img_ [class_ "imgPop", src_ "static/img/popUP.svg", onClick SwitchMenu]
                , if model.shouldShowMenu
                    then div_ [class_ "menu"]
                        $ map
                        (\x -> div_
                            [ class_ "menu-item"
                            , onClick $ Batch [PopOr, SwitchMenu, changeRoute x.routePath model.uri]
                            ] [label_ [class_ "menuMes"] [text x.label]]
                        ) menu
                    else ""
                ]
            , case uriToRouteString model.uri of 
             "" -> ""
             _  -> a_ [class_ "header-butt", href_ "#bottom"] [label_ [class_ "header-butt-label"] [text "ОСТАВИТЬ ЗАЯВКУ"]]
            ]
        _ -> 
            [ div_ [class_ "mcont"] [ img_ [class_ "imgPop", src_ "static/img/popUP.svg", onClick $ Batch [SwitchMenu, PopOr]]]
            , case uriToRouteString model.uri of 
                "" -> ""
                _ -> if model.popOr
                    then ""
                    else a_ [class_ "header-butt-mob", href_ "#bottom"] [label_ [class_ "header-butt-label"] [text "ОСТАВИТЬ ЗАЯВКУ"]]
            ]

