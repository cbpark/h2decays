{-# LANGUAGE RecordWildCards #-}

module HEP.Data.THDM.DecayWidth.H2 where

import HEP.Data.AlphaS        (alphasQ)
import HEP.Data.Constants
import HEP.Data.Kinematics    (Mass (..), betaF, massRatio, massSq)
import HEP.Data.Quark
import HEP.Data.THDM.Coupling
import HEP.Data.THDM.Model    (DecayWidth, InputParam (..))
import HEP.Data.Util          (cosBetaAlpha, dilog, lambdaF, sinBetaAlpha)

import Control.Monad.IO.Class (MonadIO)
import Data.Complex           (Complex (..), magnitude)

h2LL :: MonadIO m => Mass -> DecayWidth m
h2LL ml _ InputParam {..} = do
    let gH | mdtyp == TypeI  = gHTauTauI  angs
           | mdtyp == TypeII = gHTauTauII angs
           | otherwise     = 0

        beta = betaF mH ml
    return $ getMass mH * gH * gH * beta ** 3 / (32 * pi)

h2TauTau, h2MuMu :: MonadIO m => DecayWidth m
-- | H --> tau^+ tau^-
h2TauTau = h2LL mtau
-- | H --> mu^+ mu^-
h2MuMu   = h2LL mmu

h2BB, h2CC :: MonadIO m => DecayWidth m
-- | H --> b bbar
h2BB = h2QQ Bottom gHDD
-- | H --> c cbar
h2CC = h2QQ Charm  gHUU

h2QQ :: MonadIO m => MassiveQuark -> QuarkCoupling -> DecayWidth m
h2QQ q coup as InputParam {..} = do
    let m = getMass mH
    mqMS <- mMSbar as m q
    let gH = coup mdtyp mqMS angs
        beta = betaF mH (poleMass q)

    x <- (/pi) <$> alphasQ as m
    let nf = nLightQ q
        -- Eq.(2.11) of https://arxiv.org/abs/hep-ph/0503172
        deltaQQ = 5.67 * x
                  + (35.94 - 1.36 * nf) * x ** 2
                  + (164.14 - 25.77 * nf + 0.26 * nf * nf) * x ** 3
        mH2 = m * m
        -- Eq.(2.12) of https://arxiv.org/abs/hep-ph/0503172
        deltaH2 = (1.57
                   - 2.0 / 3 * log (mH2 / mt2)
                   + 1.0 / 9 * log (massSq mqMS / mH2) ** 2) * x ** 2

    return $ 3 * m * gH * gH * beta ** 3 / (32 * pi) * (1 + deltaQQ + deltaH2)

-- | H --> t tbar
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
        cosba = cosBetaAlpha angs
        (deltaV, x) | v == Wboson = (2, mW `massRatio` mH)
                    | otherwise   = (1, mZ `massRatio` mH)
        x2 = x * x
    return $ deltaV * m ** 3 * cosba ** 2 / (64 * pi * vEW2)
             * sqrt (1 - 4 * x2) * (1 - 4 * x2 + 12 * x2 * x2)

h2WW, h2ZZ :: MonadIO m => DecayWidth m
-- | H --> W W
h2WW = h2VV Wboson
-- | H --> Z Z
h2ZZ = h2VV Zboson

argF :: Double -> Mass -> Double -> Complex Double
argF m2 (Mass mq) coup = (coup / mq *) <$> a12 (m2 / (4 * mq * mq))

-- | H --> g g
h2GG :: MonadIO m => DecayWidth m
h2GG as InputParam {..} = do
    let m = getMass mH
    (mtMS, mbMS, mcMS) <- mMSbarHeavy as m
    alphas <- alphasQ as m

    let gHtt = gHUU mdtyp mtMS angs
        gHbb = gHDD mdtyp mbMS angs
        gHcc = gHUU mdtyp mcMS angs

        m2 = m * m
        args = (3 * vEW / (4 * sqrt2) *) <$> sum
               (zipWith (argF m2) [mtMS, mbMS, mcMS] [gHtt, gHbb, gHcc])

    return $ alphas ** 2 * m ** 3 / (144 * pi3 * vEW2) * magnitude args ** 2

-- | H --> gamma gamma
h2GaGa :: MonadIO m => DecayWidth m
h2GaGa as InputParam {..} = do
    let m = getMass mH
    (mtMS, mbMS, mcMS) <- mMSbarHeavy as m

    let gHtt = gHUU mdtyp mtMS angs
        qt2 = 4.0 / 9
        gHbb = gHDD mdtyp mbMS angs
        qb2 = 1.0 / 9
        gHcc = gHUU mdtyp mcMS angs
        qc2 = qt2
        gHTaTa = gHDD mdtyp mtau angs
        qta2 = 1

        m2 = m * m
        -- fermion contributions
        arg1 = (vEW / sqrt2 *) <$> sum
               (zipWith (argF m2)
                [mtMS, mbMS, mcMS, mtau]
                [3 * qt2 * gHtt, 3 * qb2 * gHbb, 3 * qc2 * gHcc, qta2 * gHTaTa])

        cosba = cosBetaAlpha angs
        -- W contributions
        arg2 = (cosba *) <$> a1 (m2 / (4 * massSq mW))

        mHp2 = massSq mHp
        gHp = gHHpHm mH mA mHp angs
        -- H+ contributions
        arg3 = (vEW / (sqrt2 * mHp2) * gHp *) <$> a0 (m2 / (4 * mHp2))

        args = arg1 + arg2 + arg3

    return $ alpha ** 2 * m ** 3 / (512 * pi3 * vEW2) * magnitude args ** 2

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

h2SS :: MonadIO m => Double -> Double -> Double -> DecayWidth m
h2SS beta g symF _ InputParam {..} =
    return $ symF * g ** 2 / (32 * pi * getMass mH) * beta

h2hh, h2HpHm :: MonadIO m => DecayWidth m
-- | H --> h h
h2hh   as inp@InputParam {..} =
    h2SS (betaF mH mh)  (gHhh mH mA angs)       1 as inp
-- | H --> H^+ H^-
h2HpHm as inp@InputParam {..} =
    h2SS (betaF mH mHp) (gHHpHm mH mA mHp angs) 2 as inp

-- | H --> H^+ W^-
h2HpWm :: MonadIO m => DecayWidth m
h2HpWm _ InputParam {..} = do
    let sinba = sinBetaAlpha angs
        m = getMass mH
        y = mHp `massRatio` mH
        z = mW `massRatio` mH
        lam = lambdaF 1 (y * y) (z * z)
    return $ gW2 * sinba ** 2 * m ** 3 / (64 * pi * mW2) * lam ** 1.5

h2HpWmStar :: MonadIO m => DecayWidth m
h2HpWmStar _ InputParam {..} = do
    let m = getMass mH
        m2 = m * m
        mp = getMass mHp

        c = 9 * gFermi * gFermi * mW2 ** 2 * m / (16 * pi3)
        k1 = mp * mp / m2
        k2 = mW2 / m2
        g = gFunc k1 k2
        sinba = sinBetaAlpha angs
    return $ if m < mp then 0 else c * g * sinba * sinba

gFunc :: Double -> Double -> Double
gFunc k1 k2 =
    let -- k1: \kappa_{\phi}, k2: \kappa_{V}
        lam12 = -1 + 2 * (k1 + k2) - (k1 - k2) ** 2
    in if  k1 < k2 || lam12 <= 0
       then 0
       else let sqrtLam12 = if lam12 < 0 then 0 else sqrt lam12
                x = (k2 * (1 - k2 + k1) - lam12) / ((1 - k1) * sqrtLam12)
                term1 = 2 * (-1 + k2 - k1) * sqrtLam12 * (pi / 2 + atan x)
                term2 = (lam12 - 2 * k1) * log k1
                term3 = (1 - k1) / 3 * (5 * (1 + k1) - 4 * k2 + 2 * lam12 / k2)
            in 0.25 * (term1 + term2 + term3)

x1minmax :: Double -> Double -> Double -> Double -> (Double, Double)
x1minmax kphi k1 k2 x2 =
    let kappa = 1 - x2 - kphi + k1 + k2
        term1 = kappa * (1 - x2 / 2)
        term2 = sqrt $
                (x2 * x2 / 4 - k2) * (kappa * kappa - 4 * k1 * (1 - x2 + k2))

        fac = 1 / (1 - x2 + k2)
        x1min = fac * (term1 - term2)
        x1max = fac * (term1 + term2)
    in (x1min, x1max)
