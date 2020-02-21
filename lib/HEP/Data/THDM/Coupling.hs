module HEP.Data.THDM.Coupling
    (
      gHUU
    , gHDD
    , gHLL
    , gHhh
    , gHHpHm
    , gHpUD
    , THDMType (..)
    , HffCoupling
    ) where

import HEP.Data.Constants  (gW, mW, mh2, sqrt2)
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

type HffCoupling = THDMType -> Mass -> Angles -> Double

gHUU :: HffCoupling
gHUU typ mU angs = gHff cfunc angs mU
  where
    tanb = tanBeta angs
    cosba = cosBetaAlpha angs
    cfunc sinba | typ == TypeI || typ == TypeII = cosba - sinba / tanb
                | otherwise                     = 0

gHDD :: HffCoupling
gHDD typ mD angs = gHff cfunc angs mD
  where
    tanb = tanBeta angs
    cosba = cosBetaAlpha angs
    cfunc sinba | typ == TypeI  = cosba - sinba / tanb
                | typ == TypeII = cosba + sinba * tanb
                | otherwise     = 0

gHLL :: HffCoupling
gHLL = gHDD

gHhh :: Mass  -- ^ m_H
     -> Mass  -- ^ m_A
     -> Angles
     -> Double
gHhh mH mS angs =
    let mH2  = massSq mH
        mS2  = massSq mS

        [tan2b, cosba, sinba] = ($ angs) <$>
                                [tan2Beta, cosBetaAlpha, sinBetaAlpha]

        c = gW * cosba / (2 * getMass mW)
    in c * (4 * mS2 - mH2 - 2 * mh2
            + 2 * cosba * (sinba / tan2b - cosba) * (3 * mS2 - mH2 - 2 * mh2))

gHHpHm :: Mass  -- ^ m_H
       -> Mass  -- ^ m_A
       -> Mass  -- ^ m_{H+}
       -> Angles
       -> Double
gHHpHm mH mS mHp angs =
    let mH2  = massSq mH
        mS2  = massSq mS
        mHp2 = massSq mHp

        [tan2b, cosba, sinba] = ($ angs) <$>
                                [tan2Beta, cosBetaAlpha, sinBetaAlpha]

        c = gW / (2 * getMass mW)
    in c * (cosba * (mH2 + 2 * (mHp2 - mS2)) - 2 * sinba / tan2b * (mH2 - mS2))

gHpUD :: THDMType
      -> Mass  -- ^ m_U
      -> Mass  -- ^ m_D
      -> Angles
      -> (Double, Double)
gHpUD typ mU mD angs =
    let tanb = tanBeta angs
        (md, mu) = (getMass mD, getMass mU)

        gf  | typ == TypeI  = (md - mu) / tanb
            | typ == TypeII = md * tanb + mu / tanb
            | otherwise     = 0

        gf' | typ == TypeI  = (md + mu) / tanb
            | typ == TypeII = md * tanb - mu / tanb
            | otherwise     = 0

        mw = getMass mW
        c = gW / (2 * sqrt2 * mw)
    in (c * gf, c * gf')
