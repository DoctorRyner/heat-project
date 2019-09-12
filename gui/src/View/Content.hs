module View.Content where

import Miso
import Types
import Utils
--import 

render :: Model -> View Event
render model = div_ [class_ "content"]
    [ label_ [class_ "hello"] [text $ "welcomePhrase" <-- model.locale ]
    , sukaBlator "welcomePageObogrevKrovliBtn" "obogrev-krovli"
    , sukaBlator "welcomePageObogrevPloshadiBtn" "obogrev-ploshadi"
    , sukaBlator "welcomePageObogrevTruboprovodaBtn" "obogrev-krovli"
    ]
  where
    sukaBlator txt route = div_
        [ class_ "butt"
        , onClick $ changeRoute route model.uri
        ]
        [ label_ [class_ "buttMes"] [text $ txt <-- model.locale] ]