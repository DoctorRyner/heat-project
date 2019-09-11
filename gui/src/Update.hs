module Update where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8  as BS
import qualified Data.HashMap.Strict         as HMap
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.Encoding     as TLEncoding
import           Http
import           Language.Javascript.JSaddle (valToBool)
import           Miso
import qualified Miso.String                 as MS
import           Router.URI
import           Types
import           Utils

update :: Model -> Event -> Effect Event Model
update model = \case
    -- Some init events
    Init -> batchEff model $ map pure [ FetchNormalizeCss, InitAppURI, DeviceCheck, FetchLocale ]

    -- Event that does nothing
    NoEvent -> pure model

    -- Loading css files
    FetchNormalizeCss -> withJS model $ ObtainNormalizeCss <$> Http.getLocalFile "static/css/normalize.css"
    ObtainNormalizeCss resp -> fromResp resp model $ \file -> model { files = model.files { normalizeCss = Just file } }

    -- Working with URL routing
    HandleURI uri -> pure $ model { uri = uri }
    ChangeURI uri -> model `withJS_` pushURI uri
    InitAppURI    -> Router.URI.initURI model

    -- Obtaining info about device
    DeviceCheck -> model `withJS` do
        isMobile <- valToBool =<< fromJS
            "(typeof window.orientation !== 'undefined') || (navigator.userAgent.indexOf('IEMobile') !== -1)"
        pure $ if isMobile
            then DeviceUpdate $ if model.scHeight > model.scWidth then Mobile else MobileWide
            else DeviceUpdate PC
    DeviceUpdate device -> pure model { device = device }

    -- Subscription event which updates screen info
    ScreenCheck (height, width) -> pure model { scHeight = height, scWidth = width }

    -- Obtain Locale
    FetchLocale -> withJS model $ ObtainLocale <$> Http.send get { url = "static/locale/ru.json" }
    ObtainLocale resp -> fromResp resp model $ \case
        Object hash -> do
            let _JSONStringToText = \case String res -> res; _ -> ""
                locale = HMap.map _JSONStringToText $ HMap.fromList $ filter
                    (\case
                        (_, String _) -> True
                        _             -> False
                    ) $ HMap.toList hash
            model { locale = locale }
        _           -> model
    ObtainLocaleRaw resp -> fromRespDebug resp model $ \file -> do
        let bsFile = TLEncoding.encodeUtf8 $ TL.pack $ MS.unpack file
        liftIO $ print $ HMap.fromList [ ("montegro" :: TL.Text, String "О нас") ]
        liftIO $ print $  TLEncoding.decodeUtf8 $ TLEncoding.encodeUtf8 $ TL.pack $ show $ (decode bsFile :: Maybe Value)
        pure model
