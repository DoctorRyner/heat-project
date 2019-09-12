module Types where

import           Data.Aeson
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text           as T
import           GHC.Generics
import           Miso.String         (MisoString)
import qualified Miso.String         as MS
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
    | SwitchArticleItem Int
    | ChangeArchiveArticle MisoString Int
    | NameInput MisoString
    | PhoneInput MisoString
    | PopOr
    | Batch [Event]

data Model = Model
    { files          :: Files
    , uri            :: URI
    , device         :: Device
    , isMobile       :: Bool
    , scHeight       :: Int
    , scWidth        :: Int
    , locale         :: Locale
    , shouldShowMenu :: Bool
    , article        :: Article
    , archive        :: Archive
    , name           :: MisoString
    , phone          :: MisoString
    , popOr          :: Bool
    } deriving (Show, Eq)

defaultModel :: Model
defaultModel = Model
    { files    = Files { normalizeCss = Nothing }
    , uri      = URI "" Nothing "" "" ""
    , device   = PC
    , isMobile = False
    , scHeight = 0
    , scWidth  = 0
    , locale   = HMap.empty
    , shouldShowMenu = False
    , article = article_
    , archive = defaultArchive
    , name = ""
    , phone = ""
    , popOr = False
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
    , shouldShow   :: Bool
    , imgs         :: [MisoString]
    , id_          :: Int
    } deriving (Show, Eq)

type Article = [ArticleItem]

newtype Archive = Archive [(MisoString, Article)] deriving (Show, Eq)

mkItem :: MisoString -> MisoString -> [MisoString] -> Int -> ArticleItem
mkItem title text_ = ArticleItem title text_ False

jpg :: MisoString -> MisoString
jpg name = "static/img/" <> name <> ".jpg"

defaultArchive :: Archive
defaultArchive = Archive
    [ mkArticle "obogrev-krovli"
        [ ("okrovliTitle", "okrovliText", ["static/img/okrovla.jpg"])
        , ("ovoronkiTitle", "ovoronkiText", ["static/img/ovoronka.jpg"])
        , ("ovodaTitle", "ovodaText", ["static/img/ovoda1.jpg", "static/img/ovoda2.jpg"])
        , ("okaplaTitle", "okaplaText", ["static/img/okapla.jpg"])
        , ("okrayTitle", "okrayText", ["static/img/okray.jpg"])
        , ("oendoTitle", "oendoText", ["static/img/oendo.jpg"])
        , ("oglassTitle", "oglassText", ["static/img/oglass.jpg"])
        ]
    , mkArticle "obogrev-ploshadi"
        [ ("oploshadiTitle", "oploshadiText", [ jpg "oploshadi1", jpg "oploshadi2" ])
        ]
    , mkArticle "obogrev-truboprovoda"
        [ ("otruboprovodaTitle", "otruboprovodaText", [ jpg "otruboprovoda1" ])
        ]
    , mkArticle "about"
        [ ("aboutTitle", "aboutText", [])
        ]
    , mkArticle "montage"
        [ ("montageTitle", "montageText", [ jpg "montage1" ])
        ]
    , mkArticle "planning"
        [ ("planningTitle", "planningText", [ jpg "planning1", jpg "planning2" ])
        ]
    , mkArticle "individ-proj"
        [ ("individ-projTitle", "individ-projText", [])
        ]
    ]

findArticle :: MisoString -> Archive -> Article
findArticle _    (Archive [])                    = []
findArticle name (Archive ((title, article):xs)) = if title == name
    then article
    else findArticle name $ Archive xs

mkArticle :: MisoString -> [(MisoString, MisoString, [MisoString])] -> (MisoString, Article)
mkArticle name articlesInfo = (name, (head result) {shouldShow = True} : tail result)
  where
    result = map (\((t, tex, imgs), id) -> mkItem t tex imgs id) $ zip articlesInfo [0 .. length articlesInfo]

article_ :: Article
article_ = (head mkArticle) {shouldShow = True} : tail mkArticle
  where
    articlesInfo =
        [ ("okrovliTitle", "okrovliText", ["static/img/okrovla.jpg"])
        , ("ovoronkiTitle", "ovoronkiText", ["static/img/ovoronka.jpg"])
        , ("ovodaTitle", "ovodaText", ["static/img/ovoda1.jpg", "static/img/ovoda2.jpg"])
        , ("okaplaTitle", "okaplaText", ["static/img/okapla.jpg"])
        , ("okrayTitle", "okrayText", ["static/img/okray.jpg"])
        , ("oendoTitle", "oendoText", ["static/img/oendo.jpg"])
        , ("oglassTitle", "oglassText", ["static/img/oglass.jpg"])
        ]
    mkArticle = map (\((t, tex, imgs), id) -> mkItem t tex imgs id) $ zip articlesInfo [0 .. length articlesInfo]
