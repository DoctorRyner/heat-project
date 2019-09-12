module View.ObogrevKrovli where

import           Miso  hiding (menu)
import           Types hiding (menu)
import           Utils

render :: Model -> View Event
render model = div_ [class_ "contentwall"]
    [ div_ [class_ "ewallc"] $ map
        (\item -> div_ [class_ "wrapper"]
            [ div_  [class_ "title-holder"]
                [ label_
                    [class_ "hello", onClick $ ChangeArchiveArticle "obogrev-krovli" item.id_ ] --SwitchArticleItem item.id_]
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
        ) (findArticle "obogrev-krovli" model.archive)
        ++
        [ div_ [class_ "input-cont"]
            [ input_ [class_ "input", placeholder_ "Имя*", onInput NameInput ]
            , input_ [class_ "input", placeholder_ "Телефон*", onInput PhoneInput ]
            , div_ [class_ "about-butt"] 
                [ label_ [class_ "about-label"] [text "ОСТАВИТЬ ЗАЯВКУ"]
                , a_ [name_ "bottom"] []
                ]
            ]
        ]
    ]