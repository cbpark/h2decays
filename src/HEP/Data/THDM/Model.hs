module HEP.Data.THDM.Model where

import HEP.Data.AlphaS        (AlphaS)
import HEP.Data.Kinematics    (Mass (..))
import HEP.Data.THDM.Coupling (THDMType)
import HEP.Data.Util          (Angles)

data InputParam =
    InputParam { mdtyp :: THDMType
               , mH    :: Mass
               , mA    :: Mass
               , mHp   :: Mass
               , angs  :: Angles
               } deriving Show

type DecayWidth m = AlphaS -> InputParam -> m Double
