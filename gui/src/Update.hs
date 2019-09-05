module Update where

--import           Control.Monad.IO.Class (liftIO)
import           HttpReq
import           Miso
--import           Miso.String (ms)
import           Types
import           Utils

update :: Model -> Event -> Effect Event Model
update model = \case
    NoEvent -> pure model
    GetNormalizeCss -> model `withJS` do
        normalizeCss <- HttpReq.getFileLocal $ Url "http" "localhost" 8000 "static/css/normalize.css"
        pure $ PutNormalizeCss normalizeCss
    PutNormalizeCss resp -> case resp of
        Ok file -> pure $ model { files = model.files { normalizeCss = Just file } }
        _       -> pure model
    JSTest -> model `withJS` do
        x <- HttpReq.xhrGet ""
        pure $ PutNormalizeCss $ Ok x
    Init -> batchEff model $ map pure [ JSTest ]
