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


newtype Files = Files
    { normalizeCss :: Maybe MisoString
    } deriving (Show, Eq)

data Model = Model
    { files :: Files
    , uri   :: URI
    } deriving (Show, Eq)

defaultModel :: Model
defaultModel = Model
    { files = Files { normalizeCss = Nothing }
    , uri   = URI "" Nothing "" "" ""
    }
