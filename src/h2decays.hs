module Main where

import HEP.Data.AlphaS         (mkAlphaS)
import HEP.Data.Kinematics
import HEP.Data.THDM
import HEP.Data.Util           (mkAngles)

import Data.ByteString.Builder
import System.IO               (stdout)

main :: IO ()
main = do
    as <- mkAlphaS 173.0 91.188 0.118
    -- alphasQ as 100 >>= print

    let input = InputParam { mdtyp = TypeII
                           , mH    = Mass 650
                           , mA    = Mass 650
                           , mHp   = Mass 300
                           , angs  = mkAngles 3 0.1 }

    -- brHp as input >>= print

    br1 <- renderBRH2 <$> (brH2 as input)
    hPutBuilder stdout br1
    hPutBuilder stdout (renderBRH2 Nothing)

    br2 <- renderBRHp <$> (brHp as input)
    hPutBuilder stdout br2
    hPutBuilder stdout (renderBRHp Nothing)
