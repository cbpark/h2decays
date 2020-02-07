module Main where

import HEP.Data.AlphaS     (alphasQ, mkAlphaS)
import HEP.Data.Kinematics
import HEP.Data.THDM

main :: IO ()
main = do
    as <- mkAlphaS 173.0 91.188 0.118
    alphasQ as 100 >>= print

    h2TauTau as TypeII (Mass 500) (3, 0.1) >>= print
    h2CC     as TypeII (Mass 500) (3, 0.1) >>= print
    h2BB     as TypeII (Mass 500) (3, 0.1) >>= print
    h2TT     as TypeII (Mass 500) (3, 0.1) >>= print
    h2WW     as TypeII (Mass 500) (3, 0.1) >>= print
    h2ZZ     as TypeII (Mass 500) (3, 0.1) >>= print
