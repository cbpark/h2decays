module HEP.Data.THDM.Coupling where

import HEP.Data.Constants  (gW, mW, mtau)
import HEP.Data.Kinematics (Mass (..), massRatio)
import HEP.Data.Util       (sinBetaAlpha)

data THDMType = TypeI | TypeII | UnknownType deriving Eq

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
