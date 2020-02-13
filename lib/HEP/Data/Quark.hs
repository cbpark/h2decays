module HEP.Data.Quark
    (
      MassiveQuark (..)
    , poleMass
    , nLightQ
    , mMSbar
    , mMSbarHeavy
    ) where

import HEP.Data.AlphaS        (AlphaS, alphasQ)
import HEP.Data.Constants     (mb, mc, mt)
import HEP.Data.Kinematics    (Mass (..))

import Control.Monad          (zipWithM)
import Control.Monad.IO.Class (MonadIO)

data MassiveQuark = Top | Bottom | Charm deriving Eq

poleMass :: MassiveQuark -> Mass
poleMass q | q == Top    = mt
           | q == Bottom = mb
           | q == Charm  = mc
           | otherwise   = Mass 0

nLightQ :: MassiveQuark -> Double
nLightQ q | q == Top    = 5
          | q == Bottom = 4
          | q == Charm  = 3
          | otherwise   = 0

-- | the running mass at quark pole mass.
mMSbarQ :: MonadIO m
        => AlphaS
        -> MassiveQuark
        -> m (Mass, Mass)  -- ^ (MSbar mass, pole mass)
mMSbarQ as q = do
    let mq@(Mass mQ) = poleMass q
        nf = nLightQ q

    x <- (/pi) <$> alphasQ as mQ

    let c = 1 - 4 * x / 3
            + (1.0414 * nf - 13.4434) * x ** 2
            - (0.6527 * nf * nf - 26.655 * nf + 190.595) * x ** 3
    return (Mass (mQ * c), mq)

-- | the running mass at the given scale.
mMSbar :: MonadIO m => AlphaS -> Double -> MassiveQuark -> m Mass
mMSbar as scale q = do
    (Mass mqMS, Mass mqPole) <- mMSbarQ as q

    a0 <- alphasQ as mqPole
    a1 <- alphasQ as scale
    cs <- zipWithM cAlphaRG [a0, a1] [mqPole, scale]

    return . Mass $ case cs of
                        [Just c0, Just c1] -> mqMS * c1 / c0
                        _                  -> 0

mMSbarHeavy :: MonadIO m => AlphaS -> Double -> m (Mass, Mass, Mass)
mMSbarHeavy as scale = do
    mtMS <- mMSbar as scale Top
    mbMS <- mMSbar as scale Bottom
    mcMS <- mMSbar as scale Charm
    return (mtMS, mbMS, mcMS)

cAlphaRG :: MonadIO m => Double -> Double -> m (Maybe Double)
cAlphaRG aSmu scale
    | Mass scale < mc = return Nothing
    | otherwise  = do
          let x = aSmu / pi
              (a0, c0, b0, b1, b2, b3)
                  | Mass scale < mb =
                        (25.0/6, 12.0/25, 1, 1.01413,  1.38921,  1.09054 )
                  | Mass scale < mt =
                        (23.0/6, 12.0/23, 1, 1.17549,  1.50071,  0.172478)
                  | otherwise  =
                        ( 7.0/2,  4.0/ 7, 1, 1.139796, 1.79348, -0.683433)
          return . Just $ (a0 * x) ** c0
                          * (b0 + b1 * x + b2 * x ** 2 + b3 * x ** 3)
