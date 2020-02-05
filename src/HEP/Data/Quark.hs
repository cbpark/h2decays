module HEP.Data.Quark where

import HEP.Data.AlphaS        (AlphaS, alphasQ)

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
