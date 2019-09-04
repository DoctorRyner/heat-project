{-# LANGUAGE CPP #-}

module HttpReq where

import           Control.Exception
import           Control.Lens
import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson
import           Data.Maybe
import qualified Data.ByteString.Lazy as BS
import           Miso hiding (send)
import           Miso.String
import           Types                         (Response (..))

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
            req <- liftIO $ Wreq.get (unpack url)
            let status = req ^. responseStatus . statusCode
                body'  = req ^. responseBody
            if status `elem` [ 200, 201 ]
                then case eitherDecodeStrict $ BS.toStrict body' of
                    Right res -> pure $ Types.Ok res
                    Left err  -> pure $ Types.HttpError (ms err) status 
                else pure $ Types.HttpError (ms body') 0
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
