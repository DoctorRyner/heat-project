module Http where

import           Control.Exception           (SomeException (..))
import           Control.Monad               (liftM)
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

try a = catch (Right `liftM` a) (return . Left)

send :: (FromJSON response, ToJSON response, ToJSON payload) => Request response payload -> JSM (Response response)
send = \case
    GET url headers -> do
        req <- newXMLHttpRequest
        openSimple req ("GET" :: MisoString) url
        mapM_ (uncurry $ setRequestHeader req) headers
        eitherXhrError <- try $ JSDOM.send req
        case eitherXhrError of
            Right _ -> do
                resRaw <- valToStr =<< getResponse req
                statusNum <- getStatus req
                case eitherDecodeStrict $ BSL.toStrict $ encode resRaw of
                    Right res -> do
                        status <- getStatusText req :: JSM MisoString
                        if status == "OK"
                            then pure $ Ok res
                            else pure $ HttpError (ms $ encode res) (fromEnum statusNum)
                    Left err  -> pure $ HttpError (ms err) (fromEnum statusNum)
            Left (SomeException _) -> pure $ HttpError "XHRError" 404
    POST url headers payload -> error ""
