module Style.Global where

import           Clay        hiding (menu)
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
    menuMes
-- LABEL/TEXT STYLES
    menuItem
    menuCont
    menu
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

menuCont :: Css
menuCont = element ".mcont" ? do
    height $ pct 80
    display flex
    flexDirection column
    alignItems center
    position relative

mainImgStylePop :: Css
mainImgStylePop = element ".imgPop" ? do
    height $ pct 100
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
    alignItems center
    flexDirection column

buttonMes :: Css
buttonMes = element ".buttMes" ? do
    fontSize $ px 36
    color $ rgba 255 255 255 0.67

helloMes :: Css
helloMes = element ".hello" ? do
    fontSize $ px 70
    marginTop $ pct 4
    color "#575757"

menuItem :: Css
menuItem = element ".menu-item" ? do
    width $ pct 100
    height $ px 75
    display flex
    justifyContent center
    alignItems center
    color "#414141"
    fontSize $ px 20
    hover & backgroundColor (rgba 224 224 224 0.6)
    paddingBottom $ px 0

menu :: Css
menu = element ".menu" ? do
    width $ pct 350
    height $ px 300
    backgroundColor (rgba 255 255 255 0.88)
    boxShadow' (px 0) (px 8) (px 16) (rgba 0 0 0 0.25)
    borderRadius1 5
    "backdrop-filter" -: "blur(32px)"
    position absolute
    marginTop $ pct 100
    marginLeft $ pct 125

menuMes :: Css
menuMes = element ".menuMes" ? do
    width $ pct 80
    textAlign start
--    /* Note: backdrop-filter has minimal browser support */
--    border-radius: 5px;
