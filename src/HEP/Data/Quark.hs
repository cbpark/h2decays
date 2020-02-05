module HEP.Data.Quark where

import HEP.Data.AlphaS        (AlphaS, alphasQ)

import Control.Monad          (zipWithM)
import Control.Monad.IO.Class (MonadIO)

data MassiveQuark = Top | Bottom | Charm deriving Eq

type Mass = Double

poleMass :: MassiveQuark -> Mass
poleMass q | q == Top    = 173.0
           | q == Bottom =   4.78
           | q == Charm  =   1.67
           | otherwise   =   0.0

nLight :: MassiveQuark -> Double
nLight q | q == Top    = 5
         | q == Bottom = 4
         | q == Charm  = 3
         | otherwise   = 0

-- | the running mass at quark pole mass.
mMSbarQ :: MonadIO m
        => AlphaS
        -> MassiveQuark
        -> m (Double, Double)  -- ^ (MSbar mass, pole mass)
mMSbarQ as q = do
    let mQ = poleMass q
        nf = nLight q

    x <- (/pi) <$> alphasQ as mQ

    let c = 1 - 4 * x / 3
            + (1.0414 * nf - 13.4434) * x ** 2
            - (0.6527 * nf * nf - 26.655 * nf + 190.595) * x ** 3
    return (mQ * c, mQ)

-- | the running mass at the given scale.
mMSbarMu :: MonadIO m => AlphaS -> Double -> MassiveQuark -> m Double
mMSbarMu as scale q = do
    (mqMS, mqPole) <- mMSbarQ as q

    (a0, a1) <- mapM (alphasQ as) (mqPole, scale)
    cs <- zipWithM cAlphaRG [a0, a1] [mqPole, scale]

    return $ case cs of
                 [Just c0, Just c1] -> mqMS * c1 / c0
                 _                  -> 0

cAlphaRG :: MonadIO m => Double -> Double -> m (Maybe Double)
cAlphaRG aSmu scale
    | scale < mc = return Nothing
    | otherwise  = do
          let (a0, c0, b0, b1, b2, b3)
                  | scale < mb =
                        (25.0/6, 12.0/25, 1, 1.01413,  1.38921,  1.09054 )
                  | scale < mt =
                        (23.0/6, 12.0/23, 1, 1.17549,  1.50071,  0.172478)
                  | otherwise  =
                        ( 7.0/2,  4.0/ 7, 1, 1.139796, 1.79348, -0.683433)
          return . Just $ (a0 * x) ** c0
                          * (b0 + b1 * x + b2 * x ** 2 + b3 * x ** 3)
    where
      mc = poleMass Charm
      mb = poleMass Bottom
      mt = poleMass Top
      x = aSmu / pi
