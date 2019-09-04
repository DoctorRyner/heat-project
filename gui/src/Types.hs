module Types where

import           Miso.String
import Data.Aeson
import GHC.Generics

data SomeGithubInfo = SomeGithubInfo
    { current_user_url :: MisoString
    , code_search_url  :: MisoString
    } deriving Generic

instance FromJSON SomeGithubInfo

data Response ok
    = Ok ok
    | HttpError MisoString Int

data Event
    = NoEvent
    | Init
    | GetNormalizeCss
    | PutNormalizeCss (Response SomeGithubInfo)

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
