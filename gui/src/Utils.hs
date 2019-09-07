module Utils where

import           Control.Exception           (Exception)
import           Control.Monad               (liftM)
import           Control.Monad.Catch         (MonadCatch)
import           Control.Monad.IO.Class      (liftIO)
import           Language.Javascript.JSaddle hiding (JSM, (<#))
import           Miso
import           Miso.String
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

withIO :: model -> IO event -> Effect event model
withIO model = (model <#) . liftIO

maybeStyle :: Maybe MisoString -> View Event
maybeStyle = \case
    Just cssText -> nodeHtml "style" [] [ text cssText ]
    Nothing      -> ""

try :: (MonadCatch m, Exception e) => m b -> m (Either e b)
try a = catch (Right `liftM` a) (return . Left)

fromJS :: MisoString -> JSM JSVal
fromJS = eval

fromJS' :: [MisoString] -> JSM JSVal
fromJS' = eval . Miso.String.unlines

deleteFirst :: Eq t => t -> [t] -> [t]
deleteFirst _ [] = []
deleteFirst a (b:bc) | a == b    = bc
                     | otherwise = b : deleteFirst a bc

changeRoute :: MisoString -> URI -> Event
changeRoute routeStr uri = ChangeURI $ uri { URI.uriPath = unpack routeStr }