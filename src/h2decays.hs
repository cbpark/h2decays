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

    -- hpTB       as input >>= (\x -> putStrLn $ "H+ --> t bbar:\t" ++ show x)
    -- hpCS       as input >>= (\x -> putStrLn $ "H+ --> c sbar:\t" ++ show x)
    -- hpTauNu    as input >>= (\x -> putStrLn $ "H+ --> tau nu:\t" ++ show x)
    -- hpMuNu     as input >>= (\x -> putStrLn $ "H+ --> mu nu:\t" ++ show x)
    -- hpWph      as input >>= (\x -> putStrLn $ "H+ --> W+ h:\t" ++ show x)

    brH2 as input >>= print
