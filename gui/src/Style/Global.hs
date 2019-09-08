module Style.Global where

import           Clay
import           Clay.Extra
import           Miso.String hiding (center)

css :: MisoString
css = Miso.String.ms . render $ do
    body ? pure ()
    mainHeader
    mainImgStyle
    mainImgStylePop
    contentStyle
    buttonStyle
-- LABEL/TEXT STYLES
    helloMes
    buttonMes
-- LABEL/TEXT STYLES
    pure ()

mainHeader :: Css
mainHeader = element ".header" ? do
    backgroundColor "#ffffff"
    width $ pct 100
    height $ vh 12
    -- !TODO replace boxShadow'
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
    height $ vh 10
    borderRadius1 90
    boxShadow' (px 0) (px 12) (px 15) (rgba 0 0 0 0.2 )
    marginTop $ pct 3
    display flex
    alignItems center
    justifyContent center

contentStyle :: Css 
contentStyle = element ".content" ? do
    width $ pct 100
    height $ vh 88
    display flex
--    justifyContent center
    alignItems center
    flexDirection column
    
    
--    backgroundColor "#000000"
buttonMes :: Css
buttonMes = element ".buttMes" ? do
    fontSize $ px 36
    color $ rgba 255 255 255 0.67

helloMes :: Css
helloMes = element ".hello" ? do
    fontSize $ px 70
    marginTop $ pct 4
    color "#575757"