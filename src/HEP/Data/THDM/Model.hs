module HEP.Data.THDM.Model where

import HEP.Data.AlphaS        (AlphaS)
import HEP.Data.Kinematics    (Mass (..))
import HEP.Data.THDM.Coupling (THDMType)

data InputParam =
    InputParam { mdtyp :: THDMType
               , mH    :: Mass
               , mA    :: Mass
               , mHp   :: Mass
               , angs  :: (Double, Double)  -- ^ (tan(beta), (cos(beta - alpha)))
               } deriving Show

type DecayWidth m = AlphaS -> InputParam -> m Double
