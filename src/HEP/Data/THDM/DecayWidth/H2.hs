{-# LANGUAGE RecordWildCards #-}

module HEP.Data.THDM.DecayWidth.H2 where

import HEP.Data.AlphaS        (AlphaS, alphasQ)
import HEP.Data.Constants     (mW, mZ, mtau, pi3, sqrt2, vEW, vEW2)
import HEP.Data.Kinematics    (Mass (..), betaF, massRatio, massSq)
import HEP.Data.Quark
import HEP.Data.THDM.Coupling
import HEP.Data.THDM.Model    (InputParam (..))
import HEP.Data.Util          (dilog)

import Control.Monad.IO.Class (MonadIO)
import Data.Complex           (Complex (..), magnitude)

type DecayWidth m = AlphaS -> InputParam -> m Double

h2TauTau :: MonadIO m => DecayWidth m
h2TauTau _ InputParam {..} = do
    let gH | mdtyp == TypeI  = gHTauTauI  angs
           | mdtyp == TypeII = gHTauTauII angs
           | otherwise     = 0

        beta = betaF mH mtau
    return $ getMass mH * gH * gH * beta ** 3 / (32 * pi)

h2BB, h2CC :: MonadIO m => DecayWidth m
h2BB = h2QQ Bottom
h2CC = h2QQ Charm

h2QQ :: MonadIO m => MassiveQuark -> DecayWidth m
h2QQ q as InputParam {..} = do
    let m = getMass mH
    mqMS <- mMSbar as m q
    let gH = gHDD mdtyp mqMS angs
        beta = betaF mH (poleMass q)

    x <- (/pi) <$> alphasQ as m
    let nf = nLightQ q
        -- Eq.(2.11) of https://arxiv.org/abs/hep-ph/0503172
        deltaQQ = 5.67 * x
                  + (35.94 - 1.36 * nf) * x ** 2
                  + (164.14 - 25.77 * nf + 0.26 * nf * nf) * x ** 3
        mt2 = massSq (poleMass Top)
        mH2 = m * m
        -- Eq.(2.12) of https://arxiv.org/abs/hep-ph/0503172
        deltaH2 = (1.57
                   - 2.0 / 3 * log (mH2 / mt2)
                   + 1.0 / 9 * log (massSq mqMS / mH2) ** 2) * x ** 2

    return $ 3 * m * gH * gH * beta ** 3 / (32 * pi) * (1 + deltaQQ + deltaH2)

h2TT :: MonadIO m => DecayWidth m
h2TT as InputParam {..} = do
    let m = getMass mH
    mtMS <- mMSbar as m Top
    let gH = gHUU mdtyp mtMS angs
        betaPole = betaF mH (poleMass Top)
        betaMS   = betaF mH mtMS
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

    x <- (/pi) <$> alphasQ as m
    return $ 3 * m * gH * gH * betaPole ** 3 / (32 * pi)
             * (1 + 4.0 / 3 * x * deltaH)

data EWBosons = Wboson | Zboson deriving Eq

h2VV :: MonadIO m => EWBosons -> DecayWidth m
h2VV v _ InputParam {..} = do
    let m = getMass mH
        (_, cosba) = angs
        (deltaV, x) | v == Wboson = (2, mW `massRatio` mH)
                    | otherwise   = (1, mZ `massRatio` mH)
        x2 = x * x
    return $ deltaV * m ** 3 * cosba ** 2 / (64 * pi * vEW2)
             * sqrt (1 - 4 * x2) * (1 - 4 * x2 + 12 * x2 * x2)

h2WW, h2ZZ :: MonadIO m => DecayWidth m
h2WW = h2VV Wboson
h2ZZ = h2VV Zboson

h2GG :: MonadIO m => DecayWidth m
h2GG as InputParam {..} = do
    let m = getMass mH
    alphas <- alphasQ as m
    mtMS <- mMSbar as m Top
    mbMS <- mMSbar as m Bottom
    mcMS <- mMSbar as m Charm

    let gHtt = gHUU mdtyp mtMS angs
        gHbb = gHDD mdtyp mbMS angs
        gHcc = gHUU mdtyp mcMS angs

        m2 = m * m
        argF coup (Mass mq) = (coup / mq *) <$> a12 (m2 / (4 * mq * mq))
        args = (3 * vEW / (4 * sqrt2) *) <$>
               sum (zipWith argF [gHtt, gHbb, gHcc] [mtMS, mbMS, mcMS])

    return $ alphas * m ** 3 / (144 * pi3 * vEW2) * magnitude args ** 2

a12 :: Double -> Complex Double
a12 x = (2 / (x * x) *) <$> ((x :+ 0) + (((x - 1) *) <$> ftau x))

a1 :: Double -> Complex Double
a1 x = let x2 = x * x
       in ((-1) / x2 *) <$>
          (((2 * x2 + 3 * x) :+ 0) + ((3 * (2 * x - 1) *) <$> ftau x))

a0 :: Double -> Complex Double
a0 x = (1 / (x * x) *) <$> (ftau x - (x :+ 0))

ftau :: Double -> Complex Double
ftau x | x <= 1  = asin (sqrt x) ** 2 :+ 0
       | otherwise = let y = sqrt (1 - 1 / x)
                         z = log ((1 + y) / (1 - y)) :+ (-pi)
                     in - 0.25 * z * z
