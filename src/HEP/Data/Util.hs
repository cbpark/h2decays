module HEP.Data.Util where

piHalf :: Double -> Double
piHalf th | th >=  pi12 = piHalf $! th - pi
          | th <  -pi12 = piHalf $! th + pi
          | otherwise   = th
  where pi12 = pi / 2

sinBetaAlpha :: Double  -- ^ tan(beta)
             -> Double  -- ^ cos(beta - alpha)
             -> Double
sinBetaAlpha tb cba = let b = atan tb
                          a = piHalf (b - acos cba)
                      in sin (b - a)
