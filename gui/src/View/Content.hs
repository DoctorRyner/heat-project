module View.Content where

import Miso
import Types

render :: Model -> View Event
render model = div_ [class_ "content"] 
    [ label_ [class_ "hello"] [text "Прифч, чо дел будим?"]
    , div_ [class_ "butt"] [label_ [class_ "buttMes"] [text "Слава украине!"]]
    , div_ [class_ "butt"] [label_ [class_ "buttMes"] [text "Героям слава!" ]]
    , div_ [class_ "butt"] [label_ [class_ "buttMes"] [text "Що не вымирли!"]]
    ]