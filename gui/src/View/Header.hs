module View.Header where

import Types
import Miso
import Utils

render :: Model -> View Event
render model = div_ [class_ "header"] $
    img_ [class_ "img", src_ "static/img/logo.png", onClick ChangeMob] : arg
  where
    arg = case model.device of
        PC ->
            [ div_  [class_ "mcont"]
                [ img_ [class_ "imgPop", src_ "static/img/PopUP.svg", onClick SwitchMenu]
                , if model.shouldShowMenu
                    then div_ [class_ "menu"]
                        $ map
                        (\x -> div_
                            [ class_ "menu-item"
                            , onClick $ changeRoute x.routePath model.uri
                            ] [label_ [class_ "menuMes"] [text x.label]]
                        ) menu
                    else ""
                ]
            ]
        _ -> [ div_ [class_ "mcont"] [ img_ [class_ "imgPop", src_ "static/img/PopUP.svg", onClick SwitchMenu] ]]

