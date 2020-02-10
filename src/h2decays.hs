module Main where

import HEP.Data.AlphaS         (initAlphaS)
import HEP.Data.Kinematics
import HEP.Data.THDM
import HEP.Data.Util           (mkAngles)

import Data.ByteString.Builder
import System.IO               (stdout)

main :: IO ()
main = do
    as <- initAlphaS
    -- alphasQ as 100 >>= print

    let input = InputParam { _mdtyp = TypeII
                           , _mH    = Mass 650
                           , _mA    = Mass 650
                           , _mHp   = Mass 300
                           , _angs  = mkAngles 3 0.1 }

    renderBRH2 input <$> brH2 as input >>= hPutBuilder stdout
    renderBRHp input <$> brHp as input >>= hPutBuilder stdout

-- data InputArgs w = InputArgs
--     { _mH :: w ::: Double <?> "heavy Higgs mass"
--     }
