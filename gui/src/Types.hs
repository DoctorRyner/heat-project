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
    | ObtainLocale (Response Value)
    | ObtainLocaleRaw (Response MisoString)

data Model = Model
    { files    :: Files
    , uri      :: URI
    , device   :: Device
    , scHeight :: Int
    , scWidth  :: Int
    , locale   :: Locale
    } deriving (Show, Eq)

defaultModel :: Model
defaultModel = Model
    { files    = Files { normalizeCss = Nothing }
    , uri      = URI "" Nothing "" "" ""
    , device   = PC
    , scHeight = 0
    , scWidth  = 0
    , locale   = HMap.empty
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

l :: MisoString -> Locale -> MisoString 
l key' locale = (\key -> ms $ HMap.lookupDefault key key locale) . T.pack $ unpack key'
