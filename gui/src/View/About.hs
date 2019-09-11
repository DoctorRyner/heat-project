module View.About where

import Miso hiding (menu)
--import Utils
import Types hiding (menu)

render :: Model -> View Event
render model = div_ [class_ "contentwall"]
    [ div_ [class_ "ewallc"] $ map
        (\x -> div_ [class_ "wrapper"]
            [ div_ [] [label_      [class_ "hello"] [text x.title]]
            , div_ [] [label_      [class_ "twall"] [text x.text_]]
            , div_ [class_ "iwallc"] $ map (\y -> img_ [class_ "iwall", src_ y]) x.imgs
            ]
        ) article
    ]



