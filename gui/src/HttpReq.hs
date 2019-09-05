{-# LANGUAGE CPP #-}

module HttpReq where

import           Control.Exception
import           Control.Lens
import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson
import           Data.Maybe
import qualified Data.ByteString.Lazy as BS
import           Data.Functor (void)
import           Miso hiding (send)
import           Miso.String
import           Types                         (Response (..))
import JSDOM.Custom.XMLHttpRequest hiding (send)
import qualified JSDOM.Custom.XMLHttpRequest as JSDOM
import Language.Javascript.JSaddle as JSaddle hiding (JSM)
import JSDOM.Generated.Enums

#ifdef ghcjs_HOST_OS
import           JavaScript.Web.XMLHttpRequest hiding (Method (..))
import qualified JavaScript.Web.XMLHttpRequest as Xhr
#else
import           Network.Wreq                  as Wreq
#endif

data ReqType a
    = GET MisoString
    | POST MisoString a
    deriving Show

get :: (FromJSON res, ToJSON res) => MisoString -> JSM (Types.Response res)
get url = send (GET url :: ReqType ())

post :: (FromJSON res, ToJSON res, ToJSON payload) => MisoString -> payload -> JSM (Types.Response res)
post url payload = send $ POST url payload

data Url = Url
    { protocol
    , host :: MisoString
    , port :: Int
    , path :: MisoString
    }

urlToMs :: Url -> MisoString
urlToMs url = url.protocol <> "://" <> url.host <> ":" <> ms (show url.port) <> "/" <> url.path

getFileLocal :: Url -> JSM (Types.Response MisoString)
getFileLocal url =
#ifdef ghcjs_HOST_OS
    do
    let req = Request
            { reqMethod          = Xhr.GET
            , reqURI             = urlToMs url
            , reqLogin           = Nothing
            , reqHeaders         = [("Content-Type", "application/json;charset=UTF-8")]
            , reqWithCredentials = False
            , reqData            = NoData
            }
    (try $ xhrByteString req) >>= \case
        Right respRaw -> pure $ Types.Ok $ ms $ fromJust $ contents respRaw
        Left (SomeException err) -> pure $ Types.HttpError (ms $ show err) 404
#else
    -- GHC
    do
    eitherReq <- liftIO $ try $ readFile $ unpack url.path
    case eitherReq of
        Right req -> pure $ Types.Ok $ ms req
        Left (SomeException err) -> pure $ Types.HttpError (ms $ show err) 404
#endif

send :: (FromJSON res, ToJSON res, ToJSON payload) => ReqType payload -> JSM (Types.Response res)
send reqType =
#ifdef ghcjs_HOST_OS
    do
    let (method, url, data') = case reqType of
            GET  url       -> (Xhr.GET, url, NoData)
            POST url data' -> (Xhr.POST, url, StringData $ ms $ encode data')
        req = Request
            { reqMethod          = method
            , reqURI             = url
            , reqLogin           = Nothing
            , reqHeaders         = [("Content-Type", "application/json;charset=UTF-8")]
            , reqWithCredentials = False
            , reqData            = data'
            }
    (try $ xhrByteString req) >>= \case
        Right respRaw -> case eitherDecodeStrict $ fromJust $ contents respRaw of
            Right res -> pure $ if Prelude.any (== status respRaw) [ 200, 201 ]
                then Types.Ok res
                else Types.HttpError (ms $ encode res) (status respRaw)
            Left  err -> pure $ Types.HttpError (ms err) (status respRaw)
        Left (SomeException err) -> pure $ Types.HttpError (ms $ show err) 404
#else
    -- GHC
    case reqType of
        GET  url       -> do
            eitherReq <- liftIO $ try $ Wreq.get (unpack url)
            case eitherReq of
                Right req -> do
                    let status = req ^. responseStatus . statusCode
                        body'  = req ^. responseBody
                    if status `elem` [ 200, 201 ]
                        then case eitherDecodeStrict $ BS.toStrict body' of
                            Right res -> pure $ Types.Ok res
                            Left err  -> pure $ Types.HttpError (ms err) status
                        else pure $ Types.HttpError (ms body') 0
                Left (SomeException err) -> pure $ Types.HttpError (ms $ show err) 404
        POST url data' -> do
            eitherReq <- liftIO $ try $ Wreq.post (unpack url) (toJSON data')
            case eitherReq of
                Right req -> do
                    let status = req ^. responseStatus . statusCode
                        body'  = req ^. responseBody
                    if status `elem` [ 200, 201 ]
                        then case eitherDecodeStrict $ BS.toStrict body' of
                            Right res -> pure $ Types.Ok res
                            Left err  -> pure $ Types.HttpError (ms err) status
                        else pure $ Types.HttpError (ms body') 0
                Left (SomeException err) -> pure $ Types.HttpError (ms $ show err) 404
#endif

--setMode :: ToJSString mode => XMLHttpRequest -> mode -> JSM ()
--setMode self mode = void $ self ^. jsf "setMode"

xhrGet :: MisoString -> JSM MisoString
xhrGet _url = do
    xml <- newXMLHttpRequest
    let method = "POST" :: MisoString
        url    = "https://reqres.in/api/users" :: MisoString
--    setRequestHeader xml ("Content-Type" :: MisoString) ("application/json" :: MisoString)
--    setResponseType xml XMLHttpRequestResponseTypeJson
    openSimple xml method url
    setRequestHeader xml ("Content-Type" :: MisoString) ("application/json;charset=UTF-8" :: MisoString)
    JSDOM.sendString xml ("{ \"name\": \"mo\", \"job\": \"Keker\" }" :: MisoString)
    valToStr =<< getResponse xml