module Types where

--import           Data.Aeson
--import           GHC.Generics
import           Miso.String
import           Prelude hiding (id)

data Response ok
    = Ok ok
    | HttpError MisoString Int

data Event
    = NoEvent
    | Init
    | FetchNormalizeCss
    | ObtainNormalizeCss (Response MisoString)

newtype Files = Files
    { normalizeCss :: Maybe MisoString
    } deriving (Show, Eq)

newtype Model = Model
    { files :: Files
    } deriving (Show, Eq)

defaultModel :: Model
defaultModel = Model
    { files = Files { normalizeCss = Nothing }
    }
