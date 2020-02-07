module HEP.Data.THDM.Coupling where

import HEP.Data.Constants  (gW, mW, mh2, mtau)
import HEP.Data.Kinematics (Mass (..), massRatio, massSq)
import HEP.Data.Util       (sinBetaAlpha, tan2Beta)

data THDMType = TypeI | TypeII | UnknownType deriving (Eq, Show)

gHff :: (Double -> Double)  -- ^ a function to get the coefficient
     -> (Double, Double)    -- ^ (tan(beta), cos(beta - alpha))
     -> Mass                -- ^ fermion mass
     -> Double
gHff cfunc (tanb, cosba) mf = let sinba = sinBetaAlpha tanb cosba
                                  coeff = cfunc sinba
                              in gW * (mf `massRatio` mW) * coeff

gHUU :: THDMType
     -> Mass              -- ^ fermion mass
     -> (Double, Double)  -- ^ (tan(beta), cos(beta - alpha))
     -> Double
gHUU typ mU (tanb, cosba) = gHff cfunc (tanb, cosba) mU
  where
    cfunc sinba | typ == TypeI || typ == TypeII = cosba - sinba / tanb
                | otherwise                     = 0

gHDD :: THDMType
     -> Mass              -- ^ fermion mass
     -> (Double, Double)  -- ^ (tan(beta), cos(beta - alpha))
     -> Double
gHDD typ mD (tanb, cosba) = gHff cfunc (tanb, cosba) mD
  where
    cfunc sinba | typ == TypeI  = cosba - sinba / tanb
                | typ == TypeII = cosba + sinba * tanb
                | otherwise     = 0

gHTauTauI, gHTauTauII :: (Double, Double)  -- ^ ((tan(beta), cos(beta - alpha)))
                      -> Double
gHTauTauI  = gHDD TypeI mtau
gHTauTauII = gHDD TypeII mtau

gHhh :: Mass              -- ^ m_H
     -> Mass              -- ^ m_A
     -> (Double, Double)  -- ^ (tan(beta), cos(beta - alpha))
     -> Double
gHhh mH mA (tanb, cosba) =
    let mH2  = massSq mH
        mA2  = massSq mA
        sinba = sinBetaAlpha tanb cosba
        tan2b = tan2Beta tanb
        c = gW * cosba / (2 * getMass mW)
    in c * (4 * mA2 - mH2 - 2 * mh2
            + 2 * cosba * (sinba / tan2b - cosba) * (3 * mA2 - mH2 - 2 * mh2))

gHHpHm :: Mass              -- ^ m_H
       -> Mass              -- ^ m_A
       -> Mass              -- ^ m_{H+}
       -> (Double, Double)  -- ^ (tan(beta), cos(beta - alpha))
       -> Double
gHHpHm mH mA mHp (tanb, cosba) =
    let mH2  = massSq mH
        mA2  = massSq mA
        mHp2 = massSq mHp
        sinba = sinBetaAlpha tanb cosba
        tan2b = tan2Beta tanb
        c = gW / (2 * getMass mW)
    in c * (cosba * (mH2 + 2 * (mHp2 - mA2)) - 2 * sinba / tan2b * (mH2 - mA2))
