module View.Header where

import Types
import Miso

render :: Model -> View Event
render model = nodeHtml "wrapper" [] 
    [ div_ [class_ "header"] 
        [ img_ [class_ "img", src_ "static/img/logo.png"]
        , img_ [class_ "imgPop", src_ "static/img/PopUP.svg"]
        ]
    ]