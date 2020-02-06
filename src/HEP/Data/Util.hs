module HEP.Data.Util where

piHalf :: Double -> Double
piHalf th | th >=  pi12 = piHalf $! th - pi
          | th <  -pi12 = piHalf $! th + pi
          | otherwise   = th
  where pi12 = pi / 2

sinBetaAlpha :: Double  -- ^ tan(beta)
             -> Double  -- ^ cos(beta - alpha)
             -> Double
sinBetaAlpha tanb cosba = let b = atan tanb
                              a = piHalf (b - acos cosba)
                          in sin (b - a)
