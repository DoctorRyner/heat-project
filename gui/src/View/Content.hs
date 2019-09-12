module View.Content where

import Miso
import Types
import Utils
--import 

render :: Model -> View Event
render model =
    div_ [class_ "content"]
        [ label_ [class_ "hello"] [text "ПРИВЕТСТВЕННАЯ ФРАЗА!"]
        , div_ [class_ "butt"] [label_ [class_ "buttMes"] [text "ОБОГРЕВ КРОВЛИ"]]
        , div_ [class_ "butt"] [label_ [class_ "buttMes"] [text "ОБОГРЕВ ПЛОЩАДОК" ]]
        , div_ [class_ "butt"] [label_ [class_ "buttMes"] [text "ОБОГРЕВ ТРЕБОПРОВОДА"]]
        ]
