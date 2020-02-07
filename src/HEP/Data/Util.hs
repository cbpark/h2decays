module HEP.Data.Util
    (
      Angles
    , mkAngles
    , tanBeta
    , tan2Beta
    , cosBetaAlpha
    , sinBetaAlpha
    , lambdaF
    , dilog
    ) where

newtype Angles = Angles (Double, Double) deriving Show

mkAngles :: Double -> Double -> Angles
mkAngles tanb cosba = Angles (tanb, cosba)

tanBeta :: Angles -> Double
tanBeta (Angles (tanb, _)) = tanb

tan2Beta :: Angles -> Double
tan2Beta (Angles (tanb, _)) = 2 * tanb / (1 - tanb * tanb)

cosBetaAlpha :: Angles -> Double
cosBetaAlpha (Angles (_, cosba)) = cosba

sinBetaAlpha :: Angles -> Double
sinBetaAlpha (Angles (tanb, cosba)) = let b = atan tanb
                                          a = piHalf (b - acos cosba)
                                      in sin (b - a)

piHalf :: Double -> Double
piHalf th | th >=  pi12 = piHalf $! th - pi
          | th <  -pi12 = piHalf $! th + pi
          | otherwise   = th
  where pi12 = pi / 2

lambdaF :: Double -> Double -> Double -> Double
lambdaF x y z = max 0 lam
  where lam = (x - y - z) ** 2 - 4 * y * z

foreign import ccall "gsl_sf_dilog" gsl_sf_dilog :: Double -> Double

dilog :: Double -> Double
dilog = gsl_sf_dilog
