module Main where

import HEP.Data.AlphaS     (alphasQ, mkAlphaS)
import HEP.Data.Kinematics
import HEP.Data.THDM

main :: IO ()
main = do
    as <- mkAlphaS 173.0 91.188 0.118
    alphasQ as 100 >>= print

    let input = InputParam { mdtyp = TypeII
                           , mH    = Mass 500
                           , mA    = Mass 500
                           , mHp   = Mass 200
                           , angs  = (3, 0.1)
                           }

    h2TauTau as input >>= print
    h2CC     as input >>= print
    h2BB     as input >>= print
    h2TT     as input >>= print
    h2WW     as input >>= print
    h2ZZ     as input >>= print
    h2GG     as input >>= print
