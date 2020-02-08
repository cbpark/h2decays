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

    h2TauTau   as input >>= (\x -> putStrLn $ "H --> tau tau:\t" ++ show x)
    h2MuMu     as input >>= (\x -> putStrLn $ "H --> mu mu:\t" ++ show x)
    h2CC       as input >>= (\x -> putStrLn $ "H --> c cbar:\t" ++ show x)
    h2BB       as input >>= (\x -> putStrLn $ "H --> b bbar:\t" ++ show x)
    h2TT       as input >>= (\x -> putStrLn $ "H --> t tbar:\t" ++ show x)
    h2WW       as input >>= (\x -> putStrLn $ "H --> W W:\t" ++ show x)
    h2ZZ       as input >>= (\x -> putStrLn $ "H --> Z Z:\t" ++ show x)
    h2GG       as input >>= (\x -> putStrLn $ "H --> g g:\t" ++ show x)
    h2GaGa     as input >>= (\x -> putStrLn $ "H --> ga ga:\t" ++ show x)
    h2hh       as input >>= (\x -> putStrLn $ "H --> h h:\t" ++ show x)
    h2HpHm     as input >>= (\x -> putStrLn $ "H --> H+ H-:\t" ++ show x)
    h2HpWm     as input >>= (\x -> putStrLn $ "H --> H+ W-:\t" ++ show x)
    h2HpWmStar as input >>= (\x -> putStrLn $ "H --> H+ W-*:\t" ++ show x)

    hpTB       as input >>= (\x -> putStrLn $ "H+ --> t bbar:\t" ++ show x)
    hpCS       as input >>= (\x -> putStrLn $ "H+ --> c sbar:\t" ++ show x)
    hpTauNu    as input >>= (\x -> putStrLn $ "H+ --> tau nu:\t" ++ show x)
    hpMuNu     as input >>= (\x -> putStrLn $ "H+ --> mu nu:\t" ++ show x)
    hpWph      as input >>= (\x -> putStrLn $ "H+ --> W+ h:\t" ++ show x)
