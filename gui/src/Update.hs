module Update where

import           Data.Char
import           Data.Coerce                 (coerce)
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

    SwitchMenu -> pure model { shouldShowMenu = not model.shouldShowMenu }

    ChangeMob -> model `withJS` pure (DeviceUpdate Mobile)

    SwitchArticleItem id_ ->
        let switchArticleItem item = item { shouldShow = not item.shouldShow }
        in pure model
            { article = map
                (\item -> if item.id_ == id_
                    then switchArticleItem item
                    else item
                ) model.article
            }

    ChangeArchiveArticle name id' ->
        let changeArticle = map
                (\item -> if item.id_ == id'
                    then item { shouldShow = not item.shouldShow }
                    else item
                )
            newArchive = Archive $ map
                (\(title, article) -> (title, if title == name then changeArticle article else article)
                )
                (coerce model.archive)
        in pure model { archive = newArchive }

    NameInput newName -> pure model { name = newName }

    PhoneInput newPhone -> pure model { phone = MS.filter (\x -> isDigit x || elem x ['+', ' ', '(', ')']) newPhone }

    PopOr -> pure model { popOr = not model.popOr }

    Batch events -> batchEff model $ map pure events

    -- Obtain Locale
    FetchLocale       -> withJS model $ ObtainLocale <$> Http.send get { url = "static/locale/ru.json" }
    ObtainLocale resp -> fromResp resp model $ \locale -> model { locale = locale }

