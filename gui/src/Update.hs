module Update where

--import           Control.Monad.IO.Class (liftIO)
import           Miso
import           Miso.String (ms)
import           Types
import           Utils

update :: Model -> Event -> Effect Event Model
update model = \case
    NoEvent -> pure model
    GetNormalizeCss -> pure model -- `withJS` do
--        normalizeCss <- HttpReq.getFileLocal $ Url "http" "localhost" 8000 "static/css/normalize.css"
--        pure $ PutNormalizeCss normalizeCss
    PutNormalizeCss resp -> case resp of
        Ok file -> pure $ model { files = model.files { normalizeCss = Just file } }
        HttpError err code -> model `withJS` do
            alert $ err <> " | " <> ms (show code)
            pure NoEvent
    Init -> batchEff model $ map pure []
