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
