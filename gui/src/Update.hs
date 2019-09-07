module Update where

import           Control.Exception           (SomeException (..))
import           Control.Monad               (when)
import           Data.Maybe                  (fromMaybe)
import           Http
import           Language.Javascript.JSaddle (valToStr)
import           Miso
--import           Miso.String (ms)
import qualified Miso.String                 as MS
import           Network.URI                 as URI
import           Types
import           Utils

update :: Model -> Event -> Effect Event Model
update model =
    \case
        NoEvent -> pure model
        FetchNormalizeCss -> model `withJS` do
            normalizeCss <- Http.getLocalFile "static/css/normalize.css"
            pure $ ObtainNormalizeCss normalizeCss
        ObtainNormalizeCss resp -> case resp of
            Ok file -> pure $ model {files = model.files {normalizeCss = Just file}}
            HttpError err _ ->
                model `withJS` do
                    alert err
                    pure NoEvent
        Init -> batchEff model $ map pure [FetchNormalizeCss, GetCurrentURI]
        HandleURI uri -> pure $ model {uri = uri}
        ChangeURI uri -> model `withJS` do
            pushURI uri
            pure NoEvent
        GetCurrentURI ->
            model `withJS` do
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
