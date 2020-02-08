module HEP.Data.THDM.Coupling
    (
      gHUU
    , gHDD
    , gHTauTauI
    , gHTauTauII
    , gHhh
    , gHHpHm
    , THDMType (..)
    , QuarkCoupling
    ) where

import HEP.Data.Constants  (gW, mW, mh2, mtau)
import HEP.Data.Kinematics (Mass (..), massRatio, massSq)
import HEP.Data.Util

data THDMType = TypeI | TypeII | UnknownType deriving (Eq, Show)

gHff :: (Double -> Double)  -- ^ a function to get the coefficient
     -> Angles
     -> Mass
     -> Double
gHff cfunc angs mf = let sinba = sinBetaAlpha angs
                         coeff = cfunc sinba
                     in gW * (mf `massRatio` mW) * coeff

type QuarkCoupling = THDMType -> Mass -> Angles -> Double

gHUU :: QuarkCoupling
gHUU typ mU angs = gHff cfunc angs mU
  where
    tanb = tanBeta angs
    cosba = cosBetaAlpha angs
    cfunc sinba | typ == TypeI || typ == TypeII = cosba - sinba / tanb
                | otherwise                     = 0

gHDD :: QuarkCoupling
gHDD typ mD angs = gHff cfunc angs mD
  where
    tanb = tanBeta angs
    cosba = cosBetaAlpha angs
    cfunc sinba | typ == TypeI  = cosba - sinba / tanb
                | typ == TypeII = cosba + sinba * tanb
                | otherwise     = 0

gHTauTauI, gHTauTauII :: Angles -> Double
gHTauTauI  = gHDD TypeI mtau
gHTauTauII = gHDD TypeII mtau

gHhh :: Mass  -- ^ m_H
     -> Mass  -- ^ m_A
     -> Angles
     -> Double
gHhh mH mA angs =
    let mH2  = massSq mH
        mA2  = massSq mA

        [tan2b, cosba, sinba] = ($ angs) <$>
                                [tan2Beta, cosBetaAlpha, sinBetaAlpha]

        c = gW * cosba / (2 * getMass mW)
    in c * (4 * mA2 - mH2 - 2 * mh2
            + 2 * cosba * (sinba / tan2b - cosba) * (3 * mA2 - mH2 - 2 * mh2))

gHHpHm :: Mass  -- ^ m_H
       -> Mass  -- ^ m_A
       -> Mass  -- ^ m_{H+}
       -> Angles
       -> Double
gHHpHm mH mA mHp angs =
    let mH2  = massSq mH
        mA2  = massSq mA
        mHp2 = massSq mHp

        [tan2b, cosba, sinba] = ($ angs) <$>
                                [tan2Beta, cosBetaAlpha, sinBetaAlpha]

        c = gW / (2 * getMass mW)
    in c * (cosba * (mH2 + 2 * (mHp2 - mA2)) - 2 * sinba / tan2b * (mH2 - mA2))
