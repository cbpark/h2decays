{-# LANGUAGE RecordWildCards #-}

module HEP.Data.THDM.DecayWidth.Hp where

import HEP.Data.Constants
import HEP.Data.Kinematics    (Mass (..), massRatio, massSq)
import HEP.Data.Quark         (mMSbarHeavy)
import HEP.Data.THDM.Coupling (THDMType (..))
import HEP.Data.THDM.Model    (DecayWidth, InputParam (..))
import HEP.Data.Util          (cosBetaAlpha, lambdaF, tanBeta)

import Control.Monad.IO.Class (MonadIO)

-- | H^+ --> t bbar
hpTB :: MonadIO m => DecayWidth m
hpTB as InputParam {..} = do
    let m = getMass mHp
    (mtMS, mbMS, _) <- mMSbarHeavy as m

    let tanb = tanBeta angs
        tanb2 = tanb * tanb
        [mtPole, mtMSVal, mbPole, mbMSVal] = getMass <$> [mt, mtMS, mb, mbMS]
        mtRatio2 = (mtPole / m) ** 2
        mbRatio2 = (mbPole / m) ** 2

        facCoup
            | mdtyp == TypeI  =
              (1 / tanb2 *) $
              (1  - mtRatio2 - mbRatio2)
              * (massSq mtMS + massSq mbMS)
              + 4 * mtPole * mtMSVal * mbPole * mbMSVal / (m * m)
            | mdtyp == TypeII =
              (1  - mtRatio2 - mbRatio2)
              * (massSq mtMS / tanb2 + massSq mbMS * tanb2)
              - 4 * mtPole * mtMSVal * mbPole * mbMSVal / (m * m)
            | otherwise       = 0

        coeff = 3 * gW2 * vTB * vTB * m * sqrt (lambdaF 1 mtRatio2 mbRatio2)
                / (32 * pi2 * mW2)

    return $ coeff * facCoup

-- | H^+ --> c sbar
hpCS :: MonadIO m => DecayWidth m
hpCS as InputParam {..} = do
    let m = getMass mH
    (_, _, mcMS) <- mMSbarHeavy as m

    let tanb = tanBeta angs
        facCoup | mdtyp == TypeI || mdtyp == TypeII = massSq mcMS / (tanb ** 2)
                | otherwise                         = 0

    return $ if m < getMass mcMS
             then 0
             else 3 * gW2 * vCS * vCS * m / (32 * pi2 * mW2) * facCoup

hpLL :: MonadIO m => Double -> DecayWidth m
hpLL ml2 _ InputParam {..} = do
    let tanb = tanBeta angs
        facTan | mdtyp == TypeI  = 1 / (tanb * tanb)
               | mdtyp == TypeII = tanb * tanb
               | otherwise       = 0
        gamma = gW2 * getMass mHp * ml2 / (32 * pi2 * mW2)
                * (1 - ml2 / massSq mHp) ** 2
    return $ gamma * facTan

hpTauNu, hpMuNu :: MonadIO m => DecayWidth m
-- | H^+ --> tau^+ tau^-
hpTauNu = hpLL mtau2
-- | H^+ --> mu^+ mu^-
hpMuNu  = hpLL mmu2

-- | H^+ --> W^+ h
hpWph :: MonadIO m => DecayWidth m
hpWph _ InputParam {..} = do
    let cosba = cosBetaAlpha angs
        y = mW `massRatio` mHp
        z = mh `massRatio` mHp
    return $ gW2 * cosba * cosba * getMass mHp / (64 * pi * mW2)
             * lambdaF 1 (y * y) (z * z) ** 1.5
