module Types where

--import           Data.Aeson
--import           GHC.Generics
import           Miso.String
import           Prelude hiding (id)
import Network.URI

data Response ok
    = Ok ok
    | HttpError MisoString Int

data Event
    = NoEvent
    | Init
    | FetchNormalizeCss
    | ObtainNormalizeCss (Response MisoString)
    | GetCurrentURI
    | HandleURI URI
    | ChangeURI URI
    | DeviceCheck
    | DeviceUpdate Device
    | ScreenCheck (Int, Int)

newtype Files = Files
    { normalizeCss :: Maybe MisoString
    } deriving (Show, Eq)

data Model = Model
    { files :: Files
    , uri   :: URI
    , device :: Device
    , scHeight :: Int
    , scWidth :: Int
    } deriving (Show, Eq)

defaultModel :: Model
defaultModel = Model
    { files = Files { normalizeCss = Nothing }
    , uri   = URI "" Nothing "" "" ""
    , device = PC 
    , scHeight = 0
    , scWidth = 0
    }

data Device 
    = PC 
    | Mobile
    | MobileWide
    deriving (Show, Eq)