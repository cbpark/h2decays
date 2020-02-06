module HEP.Data.THDM.Coupling where

import HEP.Data.Constants  (gW, mW)
import HEP.Data.Kinematics (Mass (..), massRatio)
import HEP.Data.Util       (sinBetaAlpha)

data THDMType = TypeI | TypeII | UnknownType deriving Eq

gHff :: (Double -> Double)  -- ^ function to get the coefficient
     -> Double              -- ^ tan(beta)
     -> Double              -- ^ cos(beta - alpha)
     -> Mass                -- ^ fermion mass
     -> Double
gHff cfunc tanb cosba mf = let sinba = sinBetaAlpha tanb cosba
                               coeff = cfunc sinba
                           in gW * (mf `massRatio` mW) * coeff

gHUU :: THDMType
     -> Mass    -- ^ fermion mass
     -> Double  -- ^ tan(beta)
     -> Double  -- ^ cos(beta - alpha)
     -> Double
gHUU typ mU tanb cosba = gHff cfunc tanb cosba mU
  where
    cfunc sinba | typ == TypeI || typ == TypeII = cosba - sinba / tanb
                | otherwise                     = 0
