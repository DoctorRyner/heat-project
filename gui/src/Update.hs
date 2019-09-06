module Update where

import           Control.Monad (when)
import           Http
import           Miso
import           Network.URI   as URI
import           Types
import           Utils

update :: Model -> Event -> Effect Event Model
update model = \case
    NoEvent -> pure model
    FetchNormalizeCss -> model `withJS` do
        normalizeCss <- Http.getLocalFile "static/css/normalize.css"
        pure $ ObtainNormalizeCss normalizeCss
    ObtainNormalizeCss resp -> case resp of
        Ok file -> pure $ model { files = model.files { normalizeCss = Just file } }
        HttpError err _ -> model `withJS` do
            alert err
            pure NoEvent
    Init -> batchEff model $ map pure [ FetchNormalizeCss, GetCurrentURI ]
    HandleURI uri -> pure $ model { uri = uri }
    ChangeURI uri -> model `withJS` do
        pushURI uri
        pure NoEvent
    GetCurrentURI -> model `withJS` do
        uri <- getCurrentURI
        let fragment    = uriFragment uri
            hasFragment = take 1 fragment == "#"
            newURI      = if hasFragment
                then do
                    -- QUEST
                    -- if we have query in fragment then we should split fragment and query
                    -- fragment should go to uriPath
                    -- query should go to uriQuery
                    uri { URI.uriPath = "/" <> tail fragment, URI.uriFragment = "", URI.uriQuery = "" }
                else uri
        when hasFragment $ pushURI newURI
        pure $ HandleURI newURI
