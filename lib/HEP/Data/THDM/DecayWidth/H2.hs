{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}

module HEP.Data.THDM.DecayWidth.H2
    (
      h2TT
    , h2BB
    , h2CC
    , h2TauTau
    , h2MuMu
    , h2WW
    , h2ZZ
    , h2GaGa
    , h2GG
    , h2hh
    , h2HpHm
    , h2HpWm
    , h2AZ
    , h2AA
    ) where

import HEP.Data.AlphaS        (alphasQ)
import HEP.Data.Constants
import HEP.Data.Kinematics    (Mass (..), betaF, massRatio, massSq)
import HEP.Data.Quark
import HEP.Data.THDM.Coupling
import HEP.Data.THDM.Model    (DecayWidth, InputParam (..))
import HEP.Data.Util

import Control.Monad.IO.Class (MonadIO)
import Data.Complex           (Complex (..), magnitude)

h2LL :: MonadIO m => Mass -> DecayWidth m
h2LL ml _ InputParam {..} = do
    let gH = gHLL _mdtyp ml _angs
        beta = betaF _mH ml
    return $ getMass _mH * gH * gH * beta ** 3 / (32 * pi)

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

h2QQ :: MonadIO m => MassiveQuark -> HffCoupling -> DecayWidth m
h2QQ q coup as InputParam {..} = do
    let m = getMass _mH
    mqMS <- mMSbar as m q
    let gH = coup _mdtyp mqMS _angs
        beta = betaF _mH (poleMass q)

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
h2TT as inp@InputParam {..} = do
    let m = getMass _mH
    mtMS <- mMSbar as m Top

    let gH = gHUU _mdtyp mtMS _angs

        mt0 = poleMass Top
    {-
        mb0 = poleMass Bottom
        [mt', mb', mw'] = fmap getMass [mt0, mb0, mW]
        mphip = 2 * mt' + 1.5
        mphim = 2 * mt' - 0.5

    if | m >= mphip                        -> h2TT2body mt0 gH as inp
       | m >  mt' + mb' + mw' && m < mphim -> h2TT3body mt0 mb0 gH inp
       | m >= mphim  && m < mphip          -> do
             let inpP = inp { _mH = Mass mphip }
                 inpM = inp { _mH = Mass mphim }
             widthP <- h2TT2body mt0 gH as inpP
             widthM <- h2TT3body mt0 mb0 gH inpM
             return $ widthP - widthM / (mphip - mphim) * (m - mphim) + widthM
       | otherwise -> return 0
    -}
    if m > 2 * getMass mt0
        then h2TT2body mt0 gH as inp
        else return 0

h2TT2body :: MonadIO m
          => Mass    -- ^ top pole mass
          -> Double  -- ^ H-t-tbar coupling
          -> DecayWidth m
h2TT2body mt0 gH as InputParam {..} = do
    let m = getMass _mH
    mtMS <- mMSbar as m Top
    let betaPole = betaF _mH mt0
        betaMS   = betaF _mH mtMS
        betaMS2  = betaMS * betaMS
        kappa    = (1 - betaMS) / (1 + betaMS)
        logKappa = log kappa

        -- Eq. (2.15) of https://arxiv.org/abs/hep-ph/0503172
        aBeta | betaMS2 > 0 =
                    (1 + betaMS2) * (4 * dilog kappa + 2 * dilog (-kappa)
                                     + 3 * logKappa * log (2 / (1 + betaMS))
                                     + 2 * logKappa * log betaMS)
                    - 3 * betaMS * log (4 / (1 - betaMS2))
                    - 4 * betaMS * log betaMS
              | otherwise = 0

        -- Eq. (2.14) of https://arxiv.org/abs/hep-ph/0503172
        deltaH | betaMS2 > 0 =
                     aBeta / betaMS
                     - (3 + 34 * betaMS2
                        - 13 * betaMS2 ** 2) / (16 * betaMS ** 3) * logKappa
                     + 3 * (7 * betaMS2 - 1) / (8 * betaMS2)
               | otherwise = 0

    x <- (/pi) <$> alphasQ as m
    return $ 3 * m * gH * gH * betaPole ** 3 / (32 * pi)
             * (1 + 4.0 / 3 * x * deltaH)

{-
h2TT3body :: MonadIO m
          => Mass    -- ^ top pole mass
          -> Mass    -- ^ bottom pole mass
          -> Double  -- ^ H-t-tbar coupling
          -> InputParam
          -> m Double
h2TT3body mt0 mb0 gH InputParam {..} = do
    let kt = (mt0 `massRatio` _mH) ** 2
        kb = (mb0 `massRatio` _mH) ** 2
        kw = ( mW `massRatio` _mH) ** 2
        x1limits = x1minmax kt kw kb

        m = getMass _mH
        gamt = (1.42 / m) ** 2

        dGamma xt xb =
            -- ((1 - xt) ** 2 * (1 - (1 - xt) - (1 - xb) + kw - 5 * kt)
            --  + 2 * kw * ((1 - xt) * (1 - xb) - kw - 2 * kt * (1 - xt)
            --              + 4 * kt * kw)
            --  - kt * (1 - xt) * (1 - xb)
            --  + kt * (1 - 4 * kt) * (2 * (1 - xt) + kw + kt))
            -- / ((1 - xt) ** 2 + gamt * kt)
            -- from Eq.(A9) of https://arxiv.org/abs/1311.7208
            (-(1 - xt) ** 2 * (1 - xt - xb - kw + kt)
             + 2 * kw * ((1 - xt) * (1 - xb) - kw)
             - kt * ((1 - xt) * (1 - xb) - 2 * (1 - xt) - kw - kt))
            / ((1 - xt) ** 2 + gamt * kt)

        g = diIntegral dGamma x1limits
                       (2 * sqrt kb, 1 - kt - kw + kb - sqrt (kw * kt))
                       1.0e-9 1000
    return $ 3 * gFermi * gH * gH * m ** 3 / (256 * pi3) * g
-}

data EWBosons = Wboson | Zboson deriving Eq

h2VV :: MonadIO m => EWBosons -> DecayWidth m
h2VV v _ InputParam {..} = do
    let (deltaV, x) | v == Wboson = (2, mW `massRatio` _mH)
                    | otherwise   = (1, mZ `massRatio` _mH)
    return $ if 2 * x > 1
             then 0
             else let x2 = x * x
                      cosba = cosBetaAlpha _angs
                      m = getMass _mH
                  in deltaV * m ** 3 * cosba ** 2 / (64 * pi * vEW2)
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
    let m = getMass _mH
    (mtMS, mbMS, mcMS) <- mMSbarHeavy as m
    alphas <- alphasQ as m

    let gHtt = gHUU _mdtyp mtMS _angs
        gHbb = gHDD _mdtyp mbMS _angs
        gHcc = gHUU _mdtyp mcMS _angs

        m2 = m * m
        args = (3 * vEW / (4 * sqrt2) *) <$> sum
               (zipWith (argF m2) [mtMS, mbMS, mcMS] [gHtt, gHbb, gHcc])

    return $ alphas ** 2 * m ** 3 / (144 * pi3 * vEW2) * magnitude args ** 2

-- | H --> gamma gamma
h2GaGa :: MonadIO m => DecayWidth m
h2GaGa as InputParam {..} = do
    let m = getMass _mH
    (mtMS, mbMS, mcMS) <- mMSbarHeavy as m

    let gHtt = gHUU _mdtyp mtMS _angs
        qt2 = 4.0 / 9
        gHbb = gHDD _mdtyp mbMS _angs
        qb2 = 1.0 / 9
        gHcc = gHUU _mdtyp mcMS _angs
        qc2 = qt2
        gHTaTa = gHDD _mdtyp mtau _angs
        qta2 = 1

        m2 = m * m
        -- fermion contributions
        arg1 = (vEW / sqrt2 *) <$> sum
               (zipWith (argF m2)
                [mtMS, mbMS, mcMS, mtau]
                [3 * qt2 * gHtt, 3 * qb2 * gHbb, 3 * qc2 * gHcc, qta2 * gHTaTa])

        cosba = cosBetaAlpha _angs
        -- W contributions
        arg2 = (cosba *) <$> a1 (m2 / (4 * massSq mW))

        mHp2 = massSq _mHp
        gHp = gHHpHm _mH _mHp _m12 _angs
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
ftau x | x <= 1    = asin (sqrt x) ** 2 :+ 0
       | otherwise = let y = sqrt (1 - 1 / x)
                         z = log ((1 + y) / (1 - y)) :+ (-pi)
                     in - 0.25 * z * z

h2SS :: MonadIO m => Double -> Double -> Double -> DecayWidth m
h2SS beta g symF _ InputParam {..} =
    return $ symF * g ** 2 / (32 * pi * getMass _mH) * beta

-- | H --> h h
h2hh :: MonadIO m => DecayWidth m
h2hh as inp@InputParam {..} =
    h2SS (betaF _mH mh) (gHhh _mH _m12 _angs) 1 as inp

-- | H --> H^+ H^-
h2HpHm :: MonadIO m => DecayWidth m
h2HpHm as inp@InputParam {..} = do
    let m  = getMass _mH
        mp = getMass _mHp

        mphip = 2 * mp + 0.5
        mphim = 2 * mp - 0.5

    if | m >= mphip              -> h2HpHm2body as inp
       | m >  mp    && m < mphim -> h2HpHm3body as inp
       | m >= mphim && m < mphip -> do
             let inpP = inp { _mH = Mass mphip }
                 inpM = inp { _mH = Mass mphim }
             widthP <- h2HpHm2body as inpP
             widthM <- h2HpHm3body as inpM
             return $ widthP - widthM / (mphip - mphim) * (m - mphim) + widthM
       | otherwise -> return 0

h2HpHm2body :: MonadIO m => DecayWidth m
h2HpHm2body as inp@InputParam {..} =
    h2SS (betaF _mH _mHp) (gHHpHm _mH _mHp _m12 _angs) 2 as inp

h2HpHm3body :: MonadIO m => DecayWidth m
h2HpHm3body as inp =
    fmap (* 2) (sum <$> sequence [ h2HpTB    as inp
                                 , h2HpCS    as inp
                                 , h2HpTauNu as inp
                                 , h2HpMuNu  as inp
                                 , h2HpWh    as inp
                                 ])

h2AA :: MonadIO m => DecayWidth m
h2AA as inp@InputParam { _mA = mA } = (0.5*) <$> h2HpHm2body as (inp { _mHp = mA})

-- | H --> H^+ (H^-* --> t b)
h2HpTB :: MonadIO m => DecayWidth m
h2HpTB as inp@InputParam {..} = do
    let m = getMass _mH
    (mtMS, mbMS, _) <- mMSbarHeavy as m
    return $ h2HpUD (mt, mtMS) (mb, mbMS) 3 vTB inp

-- | H --> H^+ (H^-* --> c s)
h2HpCS :: MonadIO m => DecayWidth m
h2HpCS as inp@InputParam {..} = do
    let m = getMass _mH
    mcMS <- mMSbar as m Charm
    return $ h2HpUD (mc, mcMS) (0, 0) 3 vCS inp

h2HpTauNu, h2HpMuNu :: MonadIO m => DecayWidth m
-- | H --> H^+ (H^-* --> tau nu)
h2HpTauNu = h2HpLNu mtau
-- | H --> H^+ (H^-* --> mu nu)
h2HpMuNu  = h2HpLNu mmu

h2HpLNu :: MonadIO m => Mass -> DecayWidth m
h2HpLNu mL _ = return . h2HpUD (mL, mL) (0, 0) 1 1

h2HpUD :: (Mass, Mass)  -- ^ (pole mass, running mass) of up-type quark
       -> (Mass, Mass)  -- ^ (pole mass, running mass) of down-type quark
       -> Double        -- ^ N_{color}
       -> Double        -- ^ V_{ud}
       -> InputParam
       -> Double
h2HpUD (mU, mUMS) (mD, mDMS) ncolor vCKM InputParam {..} =
    let [m, mp, mu, md] = fmap getMass [_mH, _mHp, mU, mD]
    in if m < mp + mu + md
       then 0
       else let gH = gHHpHm _mH _mHp _m12 _angs
                (gf, gf') = gHpUD _mdtyp mUMS mDMS _angs
                gf2  = gf * gf
                gf2' = gf' * gf'

                kp = (mp / m) ** 2
                k1 = (mu / m) ** 2
                k2 = (md / m) ** 2
                x1limits = x1minmax kp k1 k2

                dGamma x1 x2 =
                    ((gf2 + gf2') * (x1 + x2 - 1 + kp - k1 - k2)
                     - 2 * (gf2 - gf2') * sqrt (k1 * k2))
                    / ((1 - x1 - x2) ** 2 + 0.01 * kp)

                g = diIntegral dGamma x1limits
                               (2 * sqrt k2, 1 - kp - k1 + k2 - sqrt (kp * k1))
                               1.0e-9 1000
            in ncolor * gH * gH * vCKM * vCKM / (128 * pi3 * m) * g

h2HpWh :: MonadIO m => DecayWidth m
h2HpWh _ InputParam {..} = do
    let m = getMass _mH
    if _mH < mW + mh
        then return 0
        else do
            let gH = gHHpHm _mH _mHp _m12 _angs
                gV = gW * cosBetaAlpha _angs / 2

                kp = (_mHp `massRatio` _mH) ** 2
                kv = (  mW `massRatio` _mH) ** 2
                kh = (  mh `massRatio` _mH) ** 2
                x1limits = x1minmax kp kv kh

                dGamma xv xh =
                    ((xv + xh - 1 + kp - kv - kh) ** 2 - 4 * kv * kh)
                    / ((1 - xv - xh) ** 2 + 0.01 * kp)

                g = diIntegral dGamma x1limits
                               (2 * sqrt kh, 1 - kp - kv + kh - sqrt (kp * kv))
                               1.0e-9 1000
            return $ gH * gH * gV * gV * m / (256 * pi3 * mW2) * g

h2PhiV :: Mass                    -- ^ the mass of scalar boson
       -> Mass                    -- ^ the mass of vector boson
       -> (InputParam -> Double)  -- ^ for 2-body decay
       -> (InputParam -> Double)  -- ^ for 3-body decay
       -> InputParam
       -> Double
h2PhiV mPhi mV phiV2body phiV3body inp@InputParam {..} =
    let m  = getMass _mH
        mp = getMass mPhi
        mv = getMass mV

        mphip = mp + mv + 4.0
        mphim = mp + mv - 1.0

    -- the trick taken from H-COUP.
    -- see SM_Hdecay.F90 of https://arxiv.org/abs/1910.12769
    in if | m >= mphip              -> phiV2body inp
          | m >  mp    && m < mphim -> phiV3body inp
          | m >= mphim && m < mphip ->
                let inpP = inp { _mH = Mass mphip }
                    inpM = inp { _mH = Mass mphim }
                    widthP = phiV2body inpP
                    widthM = phiV3body inpM
                in widthP - widthM / (mphip - mphim) * (m - mphim) + widthM
          | otherwise -> 0

h2HpWm, h2AZ :: MonadIO m => DecayWidth m
-- | H --> H^+ W^-
h2HpWm _ inp@InputParam {..} = return $ h2PhiV _mHp mW h2HW2body h2HW3body inp
-- | H --> A Z
h2AZ   _ inp@InputParam {..} = return $ h2PhiV _mA  mZ h2AZ2body h2AZ3body inp

h2PhiV2body :: Mass  -- ^ the mass of scalar boson
            -> Mass  -- ^ the mass of vector boson
            -> InputParam
            -> Double
h2PhiV2body mPhi mV InputParam {..} =
    let sinba = sinBetaAlpha _angs
        m = getMass _mH
        y = mPhi `massRatio` _mH
        z = mV `massRatio` _mH
        lam = lambdaF 1 (y * y) (z * z)
    in gW2 * sinba ** 2 * m ** 3 / (64 * pi * mW2) * lam ** 1.5

h2HW2body, h2AZ2body :: InputParam -> Double
h2HW2body inp@InputParam {..} = h2PhiV2body _mHp mW inp
h2AZ2body inp@InputParam {..} = h2PhiV2body _mA  mZ inp

h2PhiV3body :: Double
            -> Mass  -- ^ the mass of scalar boson
            -> Mass  -- ^ the mass of vector boson
            -> InputParam
            -> Double
h2PhiV3body deltaV mPhi mV InputParam {..} =
    let m = getMass _mH
        m2 = m * m
        mp = getMass mPhi
        mv = getMass mV
        mv2 = mv * mv

        c = 3 * deltaV * gFermi * gFermi * mv2 ** 2 * m / (16 * pi3)
        k1 = mp * mp / m2
        k2 = mv2 / m2
        g = gFunc k1 k2
        sinba = sinBetaAlpha _angs
    in if m < mp then 0 else c * g * sinba ** 2

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

h2HW3body, h2AZ3body :: InputParam -> Double
h2HW3body inp@InputParam {..} = h2PhiV3body 3.0 _mHp mW inp
h2AZ3body inp@InputParam {..} =
    let cW = mW `massRatio` mZ
        sW2 = 1 - cW * cW
        deltaZ = 6 * (7.0 / 12 - 10.0 * sW2 / 9 + 40.0 * sW2 * sW2 / 27)
    in h2PhiV3body deltaZ _mA mZ inp

x1minmax :: Double -> Double -> Double -> (Double -> Double, Double -> Double)
x1minmax kphi k1 k2 =
    let kappa x2 = 1 - x2 - kphi + k1 + k2
        term1 x2 = kappa x2 * (1 - x2 / 2)
        term2 x2 = sqrt . abs $
                   (x2 * x2 / 4 - k2) * (kappa x2 ** 2 - 4 * k1 * (1 - x2 + k2))

        fac x2 = 1 / (1 - x2 + k2)
        x1min x2 = fac x2 * (term1 x2 - term2 x2)
        x1max x2 = fac x2 * (term1 x2 + term2 x2)
    in (x1min, x1max)
