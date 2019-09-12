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
    | NameInput MisoString
    | PhoneInput MisoString
    | PopOr
    | Batch [Event]

data Model = Model
    { files          :: Files
    , uri            :: URI
    , device         :: Device
    , scHeight       :: Int
    , scWidth        :: Int
    , locale         :: Locale
    , shouldShowMenu :: Bool
    , article        :: Article
    , name           :: MisoString
    , phone          :: MisoString
    , popOr          :: Bool
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
    , article = article_
    , name = ""
    , phone = ""
    , popOr = True
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

article_ :: Article
article_ = (head mkArticle) {shouldShow = True} : tail mkArticle
  where
    mkItem :: MisoString -> MisoString -> [MisoString] -> Int -> ArticleItem
    mkItem title text_ = ArticleItem title text_ False
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
