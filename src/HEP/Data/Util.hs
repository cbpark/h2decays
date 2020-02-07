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

-- | tan(2 beta)
tan2Beta :: Double -> Double
tan2Beta tanb = 2 * tanb / (1 - tanb * tanb)

lambdaF :: Double -> Double -> Double -> Double
lambdaF x y z = (x - y - z) ** 2 - 4 * y * z

foreign import ccall "gsl_sf_dilog" gsl_sf_dilog :: Double -> Double

dilog :: Double -> Double
dilog = gsl_sf_dilog
