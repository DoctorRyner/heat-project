{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TupleSections #-}

module Miso.Styled where

import           Clay                (Css, element, render, (?))
import           Data.Coerce         (coerce)
import qualified Data.HashMap.Strict as HMap
import           Data.List           (nub)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as TL
import qualified Miso
import           Miso.String         (MisoString, ms)
import           System.IO.Unsafe    (unsafePerformIO)
import           System.Random

instance Eq Css where
    s1 == s2 = render s1 == render s2
    s1 /= s2 = render s1 /= render s2

type View a = VTree a

newtype Attribute a = Attribute (Miso.Attribute a)

data VTree a
    = VNode MisoString Css [Attribute a] [VTree a]
    | VText MisoString

node :: MisoString -> [Miso.Attribute a] -> [View a] -> View a
node tag attrs = VNode tag mempty (coerce attrs)

generateHtml :: HMap.HashMap TL.Text Int -> MisoString -> View a -> Miso.View a
generateHtml _ _            (VText str)                  = Miso.text str
generateHtml cssHash uniqId (VNode tag css attrs childs) = Miso.nodeHtml
    tag
    (coerce attrs ++ case HMap.lookup (render css) cssHash of
        Just className -> [ Miso.class_ $ "_" <> uniqId <> ms className ]
        Nothing        -> []
    )
    $ map (generateHtml cssHash uniqId) childs

collectCss :: View a -> [Css]
collectCss (VText _             ) = mempty
collectCss (VNode _ css _ childs) = css : mconcat (map collectCss childs)

rnd :: () -> Int
rnd _ = unsafePerformIO $ randomRIO (0, 9999999)

toUnstyled :: View a -> Miso.View a
toUnstyled tree = Miso.div_ []
    [ Miso.nodeHtml "style" [] [ Miso.text $ ms $ mconcat $ map render renderCss ]
    , generateHtml cssHash (ms uniqId) tree
    ]
  where
    uniqId    = T.pack $ show $ rnd ()
    renderCss = map (\(css, id') -> element ("._" <> uniqId <> T.pack (show id')) ? css) cssKeyed
    cssHash   = HMap.fromList (map (\(css, id') -> (render css, id')) cssKeyed)
    cssKeyed  = map (, rnd ()) css
    css       = nub $ collectCss tree
    
toUnstyled' :: View a -> [Miso.View a]
toUnstyled' tree = [ Miso.nodeHtml "style" [] [ Miso.text $ ms $ mconcat $ map render renderCss ]
                   , generateHtml cssHash (ms uniqId) tree
                   ]
  where
    uniqId    = T.pack $ show $ rnd ()
    renderCss = map (\(css, id') -> element ("._" <> uniqId <> T.pack (show id')) ? css) cssKeyed
    cssHash   = HMap.fromList (map (\(css, id') -> (render css, id')) cssKeyed)
    cssKeyed  = map (, rnd ()) css
    css       = nub $ collectCss tree

text :: MisoString -> VTree a
text = VText

styled :: MisoString -> Css -> [Miso.Attribute a] -> [View a] -> View a
styled tag css attrs = VNode tag css (coerce attrs)
