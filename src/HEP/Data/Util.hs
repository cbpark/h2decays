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
    , midpoint
    , trapezoid
    ) where

import qualified Data.Vector.Unboxed as U

newtype Angles = Angles (Double, Double) deriving Show

mkAngles :: Double -> Double -> Angles
mkAngles tanb cosba = Angles (tanb, cosba)
{-# INLINE mkAngles  #-}

tanBeta :: Angles -> Double
tanBeta (Angles (tanb, _)) = tanb
{-# INLINE tanBeta #-}

tan2Beta :: Angles -> Double
tan2Beta (Angles (tanb, _)) = 2 * tanb / (1 - tanb * tanb)
{-# INLINE tan2Beta #-}

cosBetaAlpha :: Angles -> Double
cosBetaAlpha (Angles (_, cosba)) = cosba
{-# INLINE cosBetaAlpha #-}

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
{-# INLINE lambdaF #-}

foreign import ccall "gsl/gsl_sf_dilog.h gsl_sf_dilog" gsl_sf_dilog
    :: Double -> Double

dilog :: Double -> Double
dilog = gsl_sf_dilog

-- | Midpoint integration.
midpoint :: Int -> (Double -> Double) -> Double -> Double -> Double
midpoint n f low high = dx * U.sum (U.map f points)
  where
    dx = (high - low) / fromIntegral n
    points = U.iterateN n (+ dx) (low + dx /2)
{-# INLINE midpoint #-}

-- | Trapezoidal rule
trapezoid :: Int -> (Double -> Double) -> Double -> Double -> Double
trapezoid n f low high = (dx *) $ (f low + f high) / 2 + U.sum (U.map f points)
  where
    dx = (high - low) / fromIntegral n
    points = U.iterateN (n - 1) (+ dx) (low + dx)
{-# INLINE trapezoid #-}
