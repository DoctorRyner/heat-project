module Update where

--import           Control.Monad.IO.Class (liftIO)
import           HttpReq
import           Miso
import           Miso.String (ms)
import           Types
import           Utils

update :: Model -> Event -> Effect Event Model
update model = \case
    NoEvent -> pure model
    GetNormalizeCss -> model `withJS` do
        normalizeCss <- HttpReq.send GET "https://httpstat.us/403"
        pure $ PutNormalizeCss normalizeCss
    PutNormalizeCss resp -> case resp of
        Ok _file     -> (model { files = model.files { normalizeCss = Just "" } }) <# do
            logJS "test"
            pure NoEvent
        HttpError err code -> model `withJS` do
            alert $ err <> " | " <> ms code
            pure NoEvent
    Init -> batchEff model $ map pure [ GetNormalizeCss ]
