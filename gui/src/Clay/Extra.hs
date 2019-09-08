module Clay.Extra where

import Clay

borderRadius1 :: Double -> Css
borderRadius1 num = borderRadius (px num) (px num) (px num) (px num)
