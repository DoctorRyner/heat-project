module View.About where

import           Miso  hiding (menu)
import           Types hiding (menu)
import           Utils

render :: Model -> View Event
render model = div_ [class_ "contentwall"]
    [ div_ [class_ "ewallc"] $ map
        (\item -> div_ [class_ "wrapper"]
            [ div_  [class_ "title-holder"] 
                [ label_ 
                    [class_ "hello", onClick $ SwitchArticleItem item.id_] 
                    [text $ item.title <-- model.locale]
                ]
            , div_  [] 
                [ if item.shouldShow 
                    then label_ 
                        [class_ "twall"] 
                        [text $ item.text_ <-- model.locale]
                    else ""
                ]
            , if item.shouldShow 
                then div_  
                    [class_ "iwallc"]       
                    $ map (\img -> img_ [class_ "iwall", src_ img]) item.imgs
                else ""
            ]
        ) model.article 
        ++ 
        [ input_ [class_ "input", placeholder_ "Имя*" ]
        , input_ [class_ "input", placeholder_ "Телефон*" ]
        ]
    ]



