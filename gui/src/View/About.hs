module View.About where

import           Miso  hiding (menu)
import           Types hiding (menu)
import           Utils

render :: Model -> View Event
render model = div_ [class_ "contentwall"]
    [ div_ [class_ "ewallc"] $ map
        (\x -> div_ [class_ "wrapper"]
            [ div_  [class_ "title-holder"] [label_ [class_ "hello"] [text $ x.title <-- model.locale]]
            , div_  [] 
                [ if x.shouldShow 
                    then label_             [class_ "twall"] [text $ x.text_ <-- model.locale]
                    else ""
                ]
            , if x.shouldShow 
                then div_  [class_ "iwallc"]       $ map (\y -> img_ [class_ "iwall", src_ y]) x.imgs
                else ""
            ]
        ) article
    ]



