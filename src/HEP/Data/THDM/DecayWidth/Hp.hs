{-# LANGUAGE RecordWildCards #-}

module HEP.Data.THDM.DecayWidth.Hp where

import HEP.Data.Constants
import HEP.Data.Kinematics    (Mass (..), massRatio, massSq)
import HEP.Data.THDM.Coupling (THDMType (..))
import HEP.Data.THDM.Model    (DecayWidth, InputParam (..))
import HEP.Data.Util          (lambdaF)

import Control.Monad.IO.Class (MonadIO)

-- | H^+ --> tau^+ tau^-
hpTauTau :: MonadIO m => DecayWidth m
hpTauTau _ InputParam {..} = do
    let (tanb, _) = angs
        facTan | mdtyp == TypeI  = 1 / (tanb * tanb)
               | mdtyp == TypeII = tanb * tanb
               | otherwise       = 0
        gamma = gW2 * getMass mHp * mtau2 / (32 * pi2 * mW2)
                * (1 - mtau2 / massSq mHp) ** 2
    return $ gamma * facTan

-- | H^+ --> W^+ h
hpWph :: MonadIO m => DecayWidth m
hpWph _ InputParam {..} = do
    let (_, cosba) = angs
        y = mW `massRatio` mHp
        z = mh `massRatio` mHp
    return $ gW2 * cosba * cosba * getMass mHp / (64 * pi * mW2)
             * lambdaF 1 (y * y) (z * z) ** 1.5
