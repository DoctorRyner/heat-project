module Style.Global where

import           Clay        hiding (menu)
import           Clay.Extra
import qualified Data.Text   as T
import           Miso.String hiding (center)
import qualified Miso.String as MS
import           Types       hiding (menu)

css :: Model -> MisoString
css model = Miso.String.ms . render $ do
    body ? pure ()
    mainHeader
    contentStyle
    buttonStyle
    menuContWall
    elemWallCont
    wrapperStyle
    contentStyleWall
    menuItemMob
    aboutInput
    aboutInputContainer
    aboutButton
    headerButton
    headerButtonMob
    headerButtonLabel
-- LABEL/TEXT STYLES
    helloMes
    buttonMes
    menuMes
    textWall
    titleHolder
    aboutLabel
-- LABEL/TEXT STYLES
    menuItem
    menuCont
    menuMob
    menu
-- IMG STYLES
    mainImgStyle
    mainImgStylePop
    imgWall
    imgWallCont
-- IMG STYLES

-- TEST STYLES

-- TEST STYLES

  where
    mainImgStyle = element ".img" ? do
        width $ pct $ if model.device == Mobile
            then 70
            else 20
        height $ pct 80

    buttonStyle = element ".butt" ? do
        backgroundColor "#FF6464"
        width $ pct $ if model.device == Mobile
            then 80
            else 30
        height $ vh 10
        borderRadius1 90
        boxShadow' (px 0) (px 12) (px 15) (rgba 0 0 0 0.2 )
        marginTop $ pct $ if model.device == Mobile
            then 7
            else 3
        display flex
        alignItems center
        justifyContent center

    imgWall = element ".iwall" ? do
        width $ pct $ if model.device == Mobile
            then 100
            else 45
        borderRadius1 55
        height $ vh 30
        margin (pct 2) (pct 1) (pct 1) (pct 1)

    imgWallCont = element ".iwallc" ? do
        width $ pct 100
        display flex
        justifyContent center
        flexDirection row
        flexWrap wrapReverse

    aboutInput = element ".input" ? do
        border solid (px 3) "#D7D7D7"
        textAlign center
        marginTop $ pct 2
        fontSize $ em 1 @+@ vw 1
        width $ pct $ if model.device == Mobile
            then 100
            else 40
        height $ vh 6

    aboutButton = element ".about-butt" ? do
        borderRadius1 90
        backgroundColor $ if MS.length model.name > 3 && MS.length model.phone >= 8
            then "#95FAA5"
            else "#D7D7D7"
        boxShadow' (px 0) (px 12) (px 15) (rgba 0 0 0 0.2)
        display flex
        justifyContent center
        alignItems center
        marginTop $ pct 2
        border solid (px 3) $ if MS.length model.name > 3 && MS.length model.phone >= 8
            then "#95FAA5"
            else "#D7D7D7"
--        fontSize $ em 1 @+@ vw 1.3
        width $ pct $ if model.device == Mobile
            then 100
            else 40
        height $ vh 8

    aboutLabel = element ".about-label" ? do
        textAlign center
        color $ rgba 115 115 115 0.67
        fontSize $ em 1 @+@ vw 0.7
        width $ pct 90

    aboutInputContainer = element ".input-cont" ? do
        width $ pct 100
        marginTop $ pct 5
        paddingBottom $ px 50
        display flex
        flexDirection column
        alignItems center

    contentStyleWall = element ".contentwall" ? do
        width $ pct 100
        display flex
        transform $ translateY $ vh $ if model.device == PC || model.popOr then 12 else 24
        alignItems center
        flexDirection column
        paddingBottom $ px 0

headerButtonMob :: Css
headerButtonMob = element ".header-butt-mob" ? do
    width $ pct 100
    height $ vh 12
    marginTop $ vh 12
    textDecoration none
    backgroundColor "#FF6464"
    display flex
    justifyContent center
    alignItems center
    position fixed

headerButtonLabel :: Css
headerButtonLabel = element ".header-butt-label" ? do
    width $ pct 90
    fontSize $ em 1 @+@ vw 0.7
    textAlign center
    color (rgba 255 255 255 0.67)

headerButton :: Css
headerButton = element ".header-butt" ? do
    width $ pct 25
    height $ pct 80
    backgroundColor "#FF6464"
    textDecoration none
    display flex
    justifyContent center
    alignItems center
    borderRadius1 20
    position absolute
    left $ pct 1

titleHolder :: Css
titleHolder = element ".title-holder" ? do
    width $ pct 100
    display flex

menuContWall :: Css
menuContWall = element ".mcontw" ? do
    width $ pct 100
    display flex
    flexDirection column
    alignItems center

menuCont :: Css
menuCont = element ".mcont" ? do
    height $ pct 80
    display flex
    flexDirection column
    alignItems center
    position relative
    zIndex 1001

mainImgStylePop :: Css
mainImgStylePop = element ".imgPop" ? do
    height $ pct 100
    zIndex 1200
    position relative
    backgroundSize cover

contentStyle :: Css
contentStyle = element ".content" ? do
    width $ pct 100
    height $ vh 88
    transform $ translateY $ vh 12
    zIndex 100
    display flex
    alignItems center
    flexDirection column
    position relative

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
    zIndex 1100
    justifyContent center
    position fixed

buttonMes :: Css
buttonMes = element ".buttMes" ? do
    fontSize $ em 0.3 @+@ vw 1.3
    color $ rgba 255 255 255 0.67

helloMes :: Css
helloMes = element ".hello" ? do
    fontSize $ em 2 @+@ vw 4
    marginTop $ pct 3
    marginBottom $ pct 2.5
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

menuItemMob :: Css
menuItemMob = element ".menu-item-mob" ? do
    width $ pct 100
    height $ pct 25
    display flex
    justifyContent center
    alignItems center
    color "#414141"
    fontSize $ px 20
    hover & backgroundColor (rgba 224 224 224 0.6)
    paddingBottom $ px 0

menuMob :: Css
menuMob = element ".menuMob" ? do
    width $ pct 100
    height $ vh 88
    backgroundColor (rgba 255 255 255 0.88)
    borderRadius1 5
    "backdrop-filter" -: "blur(32px)"
    transform $ translateY $ vh 12
    zIndex 1000
    position fixed

menu :: Css
menu = element ".menu" ? do
    width $ pct 350
    height $ px 300
    backgroundColor (rgba 255 255 255 0.88)
    boxShadow' (px 0) (px 8) (px 16) (rgba 0 0 0 0.25)
    borderRadius1 5
    "backdrop-filter" -: "blur(32px)"
    position absolute
    zIndex 1001
    transform $ translateY $ px 0
    marginTop $ pct 100
    marginLeft $ pct 125

menuMes :: Css
menuMes = element ".menuMes" ? do
    width $ pct 80
    textAlign start

textWall :: Css
textWall = element ".twall" ? do
    width $ pct 100
    height $ px 0
    fontSize $ em 1 @+@ vw 0.7

elemWallCont :: Css
elemWallCont = element ".ewallc" ? do
    width $ pct 80
    display flex
    flexDirection column

wrapperStyle :: Css
wrapperStyle = element ".wrapper" ? do
    width $ pct 100
    marginTop $ pct 3
    display flex
    flexDirection column
    alignItems center
