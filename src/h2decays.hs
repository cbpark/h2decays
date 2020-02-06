module Main where

import HEP.Data.AlphaS     (alphasQ, mkAlphaS)
import HEP.Data.Kinematics
import HEP.Data.THDM

main :: IO ()
main = do
    as <- mkAlphaS 173.0 91.188 0.118
    alphasQ as 100 >>= print

    print $ gammaTauTauH2 TypeII (Mass 500) (10, 0.1)
