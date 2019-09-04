{-# LANGUAGE CPP #-}

module HttpReq where

import           Control.Exception
import           Control.Lens
import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson
import           Data.Maybe
import           Miso
import           Miso.String
import           Types                         (Response (..))

#ifdef ghcjs_HOST_OS
import           JavaScript.Web.XMLHttpRequest hiding (Method (..))
import qualified JavaScript.Web.XMLHttpRequest as Xhr
#else
import           Network.Wreq                  as Wreq
#endif

data ReqType = GET | POST deriving Show


--    do
--        response <- case reqType of
--            GET  -> liftIO $ get $ unpack reqUrl
--            POST -> liftIO $ get $ unpack reqUrl
--        let res    = response ^. responseBody
--            status = response ^. responseStatus . statusCode
--        pure $ if status == 200
--            then Types.Ok $ ms res
--            else Types.HttpError $ ms status

send :: FromJSON res => ReqType -> MisoString -> JSM (Types.Response res)
send reqType url =
#ifdef ghcjs_HOST_OS
    do
    let req = Request
            { reqMethod          = case reqType of
                GET  -> Xhr.GET
                POST -> Xhr.POST
            , reqURI             = url
            , reqLogin           = Nothing
            , reqHeaders         = []
            , reqWithCredentials = False
            , reqData            = NoData
            }
    (try $ xhrByteString req) >>= \case
        Right respRaw -> case eitherDecodeStrict $ fromJust $ contents respRaw of
            Right res -> pure $ Types.Ok res
            Left  err -> pure $ Types.HttpError (ms err) (status respRaw)
        Left (SomeException err) -> pure $ Types.HttpError (ms $ show err) 404
#else
    pure $ Types.HttpError "GHC support is not implemented yet" 228
#endif
