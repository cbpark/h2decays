module HEP.Data.THDM.Coupling where

import HEP.Data.Constants (Mass, gW, mW)
import HEP.Data.Util      (sinBetaAlpha)

data THDMType = TypeI | TypeII | UnknownType deriving Eq

gHff :: Mass                -- ^ fermion mass
     -> Double              -- ^ tan(beta)
     -> Double              -- ^ cos(beta - alpha)
     -> (Double -> Double)  -- ^ function to get the coefficient
     -> Double
gHff mf tb cba cFunc = let sinba = sinBetaAlpha tb cba
                           coeff = cFunc sinba
                       in gW * mf / mW * coeff

gHuu :: THDMType
     -> Mass    -- ^ fermion mass
     -> Double  -- ^ tan(beta)
     -> Double  -- ^ cos(beta - alpha)
     -> Double
gHuu typ mu tanb cosba = gHff mu tanb cosba cfunc
  where
    cfunc sinba | typ == TypeI || typ == TypeII = cosba - sinba / tanb
                | otherwise                     = 0
