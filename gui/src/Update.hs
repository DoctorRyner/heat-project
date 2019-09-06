module Update where

import           Http
import           Miso
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
    GetCurrentURI -> model `withJS` (HandleURI <$> getCurrentURI)