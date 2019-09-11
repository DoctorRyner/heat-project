module Types where

--import           Data.Aeson
--import           GHC.Generics
import           Miso.String
import           Network.URI
import           Prelude     hiding (id)

data Response ok
    = Ok ok
    | HttpError MisoString Int

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
    | SwitchMenu
    | ChangeMob
    
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

data ArticleItem = ArticleItem { title, text_ :: MisoString, imgs :: [MisoString] }

type Article = [ArticleItem] 

article :: Article
article = 
    [ ArticleItem "Я заголовок" "Я текст" ["static/img/okrovla.jpg" ]
    , ArticleItem "Я заголовок" "Я текст" ["static/img/ovoronka.jpg"]
    , ArticleItem "Я заголовок" "Я текст" ["static/img/ovoda1.jpg", "static/img/ovoda2.jpg"]
    , ArticleItem "Я заголовок" "Я текст" ["static/img/okapla.jpg"  ]
    , ArticleItem "Я заголовок" "Я текст" ["static/img/okray.jpg"   ]
    , ArticleItem "Я заголовок" "Я текст" ["static/img/oendo.jpg"   ]
    , ArticleItem "Я заголовок" "Я текст" ["static/img/oglass.jpg"  ]
    ]

data Model = Model
    { files    :: Files
    , uri      :: URI
    , device   :: Device
    , scHeight :: Int
    , scWidth  :: Int
    , shouldShowMenu :: Bool
    } deriving (Show, Eq)

defaultModel :: Model
defaultModel = Model
    { files    = Files { normalizeCss = Nothing }
    , uri      = URI "" Nothing "" "" ""
    , device   = PC
    , scHeight = 0
    , scWidth  = 0
    , shouldShowMenu = False
    }

data Device
    = PC
    | Mobile
    | MobileWide
    deriving (Show, Eq)
