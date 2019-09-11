module Types where

import           Data.Aeson
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text           as T
import           GHC.Generics
import           Miso.String
import           Network.URI
import           Prelude             hiding (id)

data Event
    = NoEvent
    | Init
    | FetchNormalizeCss
    | ObtainNormalizeCss (Response MisoString)
    | InitAppURI
    | HandleURI URI
    | ChangeURI URI
    | DeviceCheck
    | DeviceUpdate Device
    | ScreenCheck (Int, Int)
    | FetchLocale
    | ObtainLocale (Response Locale)
    | SwitchMenu
    | ChangeMob
--    | SwitchArticle

data Model = Model
    { files             :: Files
    , uri               :: URI
    , device            :: Device
    , scHeight          :: Int
    , scWidth           :: Int
    , locale            :: Locale
    , shouldShowMenu    :: Bool
--    , articleItems :: ArticleItem
    } deriving (Show, Eq)

defaultModel :: Model
defaultModel = Model
    { files    = Files { normalizeCss = Nothing }
    , uri      = URI "" Nothing "" "" ""
    , device   = PC
    , scHeight = 0
    , scWidth  = 0
    , locale   = HMap.empty
    , shouldShowMenu = False
--    , articleItem = ArticleItem { shouldShow = True}
    }

data Device
    = PC
    | Mobile
    | MobileWide
    deriving (Show, Eq)

data Response ok
    = Ok ok
    | HttpError MisoString Int
    deriving Generic

instance ToJSON (Response Value)
instance FromJSON (Response Value)

type Locale = HMap.HashMap T.Text T.Text

newtype Files = Files
    { normalizeCss :: Maybe MisoString
    } deriving (Show, Eq)

data MenuItem = MenuItem { routePath, label :: MisoString }

type Menu = [MenuItem]

menu :: Menu
menu =
    [ MenuItem "about"        "О нас"
    , MenuItem "planning"     "Проектирование"
    , MenuItem "montage"      "Монтажные работы"
    , MenuItem "individ-proj" "Индивидуальный проект"
    ]

data ArticleItem = ArticleItem 
    { title, text_ :: MisoString
    , shouldShow :: Bool
    ,  imgs :: [MisoString] 
    } 

type Article = [ArticleItem]

article :: Article
article =
    [ ArticleItem "okrovliTitle"  "okrovliText"  True  [ "static/img/okrovla.jpg"  ]
    , ArticleItem "ovoronkiTitle" "ovoronkiText" False [ "static/img/ovoronka.jpg" ]
    , ArticleItem "ovodaTitle"    "ovodaText"    False [ "static/img/ovoda1.jpg" 
                                                       , "static/img/ovoda2.jpg"   ]
    , ArticleItem "okaplaTitle"   "okaplaText"   False [ "static/img/okapla.jpg"   ]
    , ArticleItem "okrayTitle"    "okrayText"    False [ "static/img/okray.jpg"    ]
    , ArticleItem "oendoTitle"    "oendoText"    False [ "static/img/oendo.jpg"    ]
    , ArticleItem "oglassTitle"   "oglassText"   False [ "static/img/oglass.jpg"   ]
    ]
