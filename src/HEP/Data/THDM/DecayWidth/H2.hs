module HEP.Data.THDM.DecayWidth.H2 where

import HEP.Data.AlphaS        (AlphaS, alphasQ)
import HEP.Data.Constants     (mW, mZ, mtau, vEW2)
import HEP.Data.Kinematics    (Mass (..), betaF, massRatio, massSq)
import HEP.Data.Quark
import HEP.Data.THDM.Coupling
import HEP.Data.Util          (dilog)

import Control.Monad.IO.Class (MonadIO)
import Data.Complex           (Complex (..))

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

gammaTTH2 :: MonadIO m
          => AlphaS -> THDMType -> Mass -> (Double, Double) -> m Double
gammaTTH2 as typ m@(Mass mH) angles = do
    mtMS <- mMSbar as mH Top
    let gH = gHUU typ mtMS angles
        betaPole = betaF m (poleMass Top)
        betaMS   = betaF m mtMS
        betaMS2  = betaMS * betaMS
        kappa    = (1 - betaMS) / (1 + betaMS)
        logKappa = log kappa

        -- Eq. (2.15) of https://arxiv.org/abs/hep-ph/0503172
        aBeta = (1 + betaMS2)
                * (4 * dilog kappa + 2 * dilog (-kappa)
                   + 3 * logKappa * log (2 / (1 + betaMS))
                   + 2 * logKappa * log betaMS)
                - 3 * betaMS * log (4 / (1 - betaMS2)) - 4 * betaMS * log betaMS

        -- Eq. (2.14) of https://arxiv.org/abs/hep-ph/0503172
        deltaH = aBeta / betaMS
                 - (3 + 34 * betaMS2
                    - 13 * betaMS2 ** 2) / (16 * betaMS ** 3) * logKappa
                 + 3 * (7 * betaMS2 - 1) / (8 * betaMS2)

    x <- (/pi) <$> alphasQ as mH
    return $ 3 * mH * gH * gH * betaPole ** 3 / (32 * pi)
             * (1 + 4.0 / 3 * x * deltaH)

data EWBosons = Wboson | Zboson deriving Eq

gammaVVH2 :: MonadIO m
          => EWBosons -> AlphaS -> THDMType -> Mass -> (Double, Double) -> m Double
gammaVVH2 v _ _ m@(Mass mH) (_, cosba) = do
    let (deltaV, x) | v == Wboson = (2, mW `massRatio` m)
                    | otherwise   = (1, mZ `massRatio` m)
        x2 = x * x
    return $ deltaV * mH ** 3 * cosba ** 2 / (64 * pi * vEW2)
             * sqrt (1 - 4 * x2) * (1 - 4 * x2 + 12 * x2 * x2)

gammaWWH2, gammaZZH2 :: MonadIO m
                     => AlphaS -> THDMType -> Mass -> (Double, Double) -> m Double
gammaWWH2 = gammaVVH2 Wboson
gammaZZH2 = gammaVVH2 Zboson

ftau :: Double -> Complex Double
ftau tau | tau <= 1  = asin (sqrt tau) ** 2 :+ 0
         | otherwise = let x = sqrt (1 - 1 / tau)
                           arg = log ((1 + x) / (1 - x)) :+ (-pi)
                       in - 0.25 * arg * arg
