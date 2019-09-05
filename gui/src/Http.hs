module Http where

import           Control.Exception
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8  as BSL
import           JSDOM.Custom.XMLHttpRequest as JSDOM hiding (error)
import           Language.Javascript.JSaddle hiding (JSM)
import           Miso
import           Miso.String
import           Types

data Request response payload
    = GET
        { url     :: MisoString
        , headers :: [(MisoString, MisoString)]
        }
    | POST
        { url     :: MisoString
        , headers :: [(MisoString, MisoString)]
        , payload :: Maybe payload
        }

get :: FromJSON resp => Request resp ()
get = GET
    { url     = ""
    , headers = [("Content-Type", "application/json")]
    }

post :: (FromJSON response, ToJSON payload) => Request response payload
post = POST
    { url     = ""
    , headers = [("Content-Type", "application/json")]
    , payload = Nothing
    }

send :: (FromJSON response, ToJSON response, ToJSON payload) => Request response payload -> JSM (Response response)
send = \case
    GET url headers -> do
        req <- newXMLHttpRequest
        openSimple req ("GET" :: MisoString) url
        mapM_ (uncurry $ setRequestHeader req) headers
        JSDOM.send req 
        resRaw <- valToStr =<< getResponse req
        case eitherDecodeStrict $ BSL.toStrict $ encode resRaw of
            Right res -> pure $ Ok res
            Left err  -> pure $ HttpError (ms err) 0
    POST url headers payload -> error ""
