{-# LANGUAGE CPP #-}

module Router.URI where

import           Control.Exception           (SomeException (..))
import           Control.Monad               (when)
import           Data.Maybe
import           Language.Javascript.JSaddle (valToStr)
import           Miso                        hiding (model)
import qualified Miso.String                 as MS
import           Network.URI                 as URI
import           Types
import           Utils

initURI :: Model -> Effect Event Model
initURI model = model `withJS` do
    #ifdef ghcjs_HOST_OS
    uri <- getCurrentURI
    pure $ HandleURI uri
    #else
    let uriCorrector uri = do
            let fragment = uriFragment uri
                hasFragment = take 1 fragment == "#"
                newURI =
                    if hasFragment
                        then do
                            let uriPath = "/" <> tail fragment
                            fromMaybe uri $
                                URI.parseURI $ show $ uri {URI.uriPath = uriPath, URI.uriFragment = ""}
                        else uri
            when hasFragment $ pushURI newURI
            pure $ HandleURI newURI
    try getCurrentURI >>= \case
        Right uri -> uriCorrector uri
        Left (SomeException _) -> do
            uriRaw <- valToStr =<< fromJS "window.location.href"
            let correctedUriString = deleteFirst '#' $ MS.unpack uriRaw
            case URI.parseURI correctedUriString of
                Just newURI -> do
                    pushURI newURI
                    pure $ HandleURI newURI
                Nothing -> logJS ("Error in parsing: " <> uriRaw) >> pure NoEvent
#endif
