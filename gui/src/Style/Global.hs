module Style.Global where

import           Clay
import           Clay.Color
import           Clay.Extra
import           Miso.String hiding (center)

css :: MisoString
css = Miso.String.ms . render $ do
    body ? pure ()
    mainHeader
    mainImgStyle
    mainImgStylePop
    pure ()

mainHeader :: Css
mainHeader = element ".header" ? do
    backgroundColor "#ffffff"
    width $ pct 100
    height $ vw 8
    boxShadow' (px 0) (px 4) (px 20) (rgba 0 0 0 0.25 )
    display flex
    flexDirection row
    alignItems center
    justifyContent center

mainImgStylePop :: Css
mainImgStylePop = element ".imgPop" ? do
    height $ pct 80
    backgroundSize cover

mainImgStyle :: Css
mainImgStyle = element ".img" ? do
    width $ pct 20
    height $ pct 80

buttonStyle :: Css
buttonStyle = element ".butt" ? do
    backgroundColor "#FF6464"
    width $ pct 30
    height $ vw 8
    borderRadius1 90
