module Utils where

import           Control.Exception           (Exception)
import           Control.Monad               (liftM)
import           Control.Monad.Catch         (MonadCatch)
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.HashMap.Lazy           as HMap
import qualified Data.Text                   as T
import           Language.Javascript.JSaddle hiding (JSM, (!!), (<#))
import           Miso
import           Miso.String                 (MisoString, ms, unpack)
import qualified Miso.String                 as MS
import           Network.URI                 as URI
import           Types

mshow :: Show a => a -> MisoString
mshow = ms . show

logJS :: MisoString -> JSM ()
logJS str = consoleLog =<< val (unpack str)

logJS' :: Show a => a -> JSM ()
logJS' str = consoleLog =<< val (show str)

withJS :: model -> JSM event -> Effect event model
withJS = (<#)

withJS_ :: model -> JSM () -> Effect Event model
withJS_ model action = model <# (action >> pure NoEvent)

withIO :: model -> IO event -> Effect event model
withIO model = (model <#) . liftIO

withIO_ :: model -> IO () -> Effect Event model
withIO_ model action = model <# (liftIO action >> pure NoEvent)

maybeStyle :: Maybe MisoString -> View Event
maybeStyle = \case
    Just cssText -> nodeHtml "style" [] [ text cssText ]
    Nothing      -> ""

try :: (MonadCatch m, Exception e) => m b -> m (Either e b)
try a = catch (Right `liftM` a) (return . Left)

fromJS :: MisoString -> JSM JSVal
fromJS = eval

fromJS' :: [MisoString] -> JSM JSVal
fromJS' = eval . MS.unlines

deleteFirst :: Eq t => t -> [t] -> [t]
deleteFirst _ [] = []
deleteFirst a (b:bc) | a == b    = bc
                     | otherwise = b : deleteFirst a bc

changeRoute :: MisoString -> URI -> Event
changeRoute routeStr uri = ChangeURI $ uri { URI.uriPath = unpack routeStr }

fromResp :: Response response -> model -> (response -> model) -> Effect Event model
fromResp response model updator = case response of
    Ok resp         -> pure $ updator resp
    HttpError err _ -> model `withJS_` logJS err

fromRespDebug :: Response response -> model -> (response -> JSM model) -> Effect Event model
fromRespDebug response model updator = case response of
    Ok resp         -> model `withJS` (updator resp >> pure NoEvent)
    HttpError err _ -> model `withJS_` logJS err

uriToRouteString :: URI -> String
uriToRouteString = eraseSlashAtPathEdges . uriPath where
    eraseSlashAtPathEdges str = if str == "" || str == "/"
        then ""
        else tail $ (if last str == '/' then init else id) str

-- Extract value from locale
fromLocale :: MisoString -> Locale -> MisoString
fromLocale key' locale = (\key -> ms $ HMap.lookupDefault "" key locale) . T.pack $ unpack key'

(<--) :: MisoString -> Locale -> MisoString
(<--) = fromLocale

--textBraker :: MisoString -> View a
--textBraker txt = label_ [] $ map
--    ()
