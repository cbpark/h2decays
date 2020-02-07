module Main where

import HEP.Data.AlphaS     (alphasQ, mkAlphaS)
import HEP.Data.Kinematics
import HEP.Data.THDM
import HEP.Data.Util       (mkAngles)

main :: IO ()
main = do
    as <- mkAlphaS 173.0 91.188 0.118
    alphasQ as 100 >>= print

    let input = InputParam { mdtyp = TypeII
                           , mH    = Mass 650
                           , mA    = Mass 650
                           , mHp   = Mass 300
                           , angs  = mkAngles 3 0.1 }

    h2TauTau as input >>= print
    h2MuMu   as input >>= print
    h2CC     as input >>= print
    h2BB     as input >>= print
    h2TT     as input >>= print
    h2WW     as input >>= print
    h2ZZ     as input >>= print
    h2GG     as input >>= print
    h2GaGa   as input >>= print
    h2hh     as input >>= print
    h2HpHm   as input >>= print
    h2HpWm   as input >>= print

    hpTB     as input >>= print
    hpCS     as input >>= print
    hpTauTau as input >>= print
    hpMuMu   as input >>= print
    hpWph    as input >>= print
