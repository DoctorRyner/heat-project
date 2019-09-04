module Types where

import           Data.Aeson
import           GHC.Generics
import           Miso.String

data SomeGithubInfo = SomeGithubInfo
    { current_user_url :: MisoString
    , code_search_url  :: MisoString
    } deriving Generic

instance FromJSON SomeGithubInfo
instance ToJSON SomeGithubInfo

data AmoAuthBody = AmoAuthBody
    { em :: MisoString
    , password :: MisoString
    } deriving Generic
instance ToJSON AmoAuthBody

newtype TestResp = TestResp { token :: MisoString } deriving Generic
instance FromJSON TestResp
instance ToJSON TestResp

data Response ok
    = Ok ok
    | HttpError MisoString Int

data Event
    = NoEvent
    | Init
    | GetNormalizeCss
    | PutNormalizeCss (Response SomeGithubInfo)
    | AmoAuthReq
    | AmoAuthRes (Response TestResp)

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
