module HEP.Data.THDM.DecayWidth.H2 where

import HEP.Data.AlphaS        (AlphaS, alphasQ)
import HEP.Data.Constants     (mtau)
import HEP.Data.Kinematics    (Mass (..), betaF, massSq)
import HEP.Data.Quark
import HEP.Data.THDM.Coupling

import Control.Monad.IO.Class (MonadIO)

gammaTauTauH2 :: MonadIO m
              => AlphaS -> THDMType -> Mass -> (Double, Double) -> m Double
gammaTauTauH2 _ typ m@(Mass mH) angles = do
    let gH | typ == TypeI  = gHTauTauI  angles
           | typ == TypeII = gHTauTauII angles
           | otherwise     = 0

        beta = betaF m mtau
    return $ mH * gH * gH * beta ** 3 / (32 * pi)

gammaBBH2, gammaCCH2 :: MonadIO m
                     => AlphaS -> THDMType -> Mass -> (Double, Double) -> m Double
gammaBBH2 = gammaQQH2 Bottom
gammaCCH2 = gammaQQH2 Charm

gammaQQH2 :: MonadIO m
          => MassiveQuark -> AlphaS -> THDMType -> Mass -> (Double, Double)
          -> m Double
gammaQQH2 q as typ m@(Mass mH) angles = do
    mqMS <- mMSbar as mH q
    let gH = gHDD typ mqMS angles
        beta = betaF m (poleMass q)

    x <- (/pi) <$> alphasQ as mH
    let nf = nLightQ q
        -- Eq.(2.11) of https://arxiv.org/abs/hep-ph/0503172
        deltaQQ = 5.67 * x
                  + (35.94 - 1.36 * nf) * x ** 2
                  + (164.14 - 25.77 * nf + 0.26 * nf * nf) * x ** 3
        mt2 = massSq (poleMass Top)
        mH2 = mH * mH
        -- Eq.(2.12) of https://arxiv.org/abs/hep-ph/0503172
        deltaH2 = (1.57
                   - 2.0 / 3 * log (mH2 / mt2)
                   + 1.0 / 9 * log (massSq mqMS / mH2) ** 2) * x ** 2

    return $ 3 * mH * gH * gH * beta ** 3 / (32 * pi) * (1 + deltaQQ + deltaH2)
