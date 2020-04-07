{-# LANGUAGE DeriveGeneric #-}

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
    , diIntegral
    -- , midpoint
    -- , trapezoid
    , mkPoints
    ) where

import Data.Vector             (Vector, generate)
import Numeric.GSL.Integration (integrateQAGS)

import Data.Hashable           (Hashable)

import GHC.Generics            (Generic)

newtype Angles = Angles (Double, Double) deriving (Generic, Show)

instance Hashable Angles

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

diIntegral :: (Double -> Double -> Double)          -- ^ differential decay width
           -> (Double -> Double, Double -> Double)  -- ^ limits of inner integral
           -> (Double, Double)                      -- ^ limits of outer integral
           -> Double                                -- ^ precision (eg. 1e-9)
           -> Int                                   -- ^ size of workspace
           -> Double
diIntegral dGamma (x1min, x1max) (x2min, x2max) prec size = g
  where
    -- f x2 = midpoint 1000 (`dGamma` x2) (x1min x2) (x1max x2)
    f x2 = fst $ integrateQAGS prec size (`dGamma` x2) (x1min x2) (x1max x2)
    g    = fst $ integrateQAGS prec size f x2min x2max

{-
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
-}

mkPoints :: Double -> [Double] -> (Vector Double, Int)
mkPoints stepsize vs =
    (generate npoints (\i -> vmin + fromIntegral i * stepsize), npoints)
    where (vmin, vmax) = (,) <$> minimum <*> maximum $ vs
          npoints = floor $ (vmax - vmin) / stepsize + 1
