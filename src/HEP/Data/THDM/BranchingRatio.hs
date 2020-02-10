module HEP.Data.THDM.BranchingRatio where

import HEP.Data.AlphaS          (AlphaS)
import HEP.Data.THDM.DecayWidth
import HEP.Data.THDM.Model      (InputParam (..))

import Control.Monad.IO.Class   (MonadIO)

data BRH2 = BRH2 { _totalWidth :: Double
                 , _h2TT       :: Double
                 , _h2BB       :: Double
                 , _h2CC       :: Double
                 , _h2TauTau   :: Double
                 , _h2MuMu     :: Double
                 , _h2WW       :: Double
                 , _h2ZZ       :: Double
                 , _h2GaGa     :: Double
                 , _h2GG       :: Double
                 , _h2hh       :: Double
                 , _h2HpHm     :: Double
                 , _h2HW       :: Double
                 , _h2HWstar   :: Double
                 } deriving Show

brH2 :: MonadIO m => AlphaS -> InputParam -> m (Maybe BRH2)
brH2 as inp = do
    gamH2TT       <- h2TT       as inp
    gamH2BB       <- h2BB       as inp
    gamH2CC       <- h2CC       as inp
    gamH2TauTau   <- h2TauTau   as inp
    gamH2MuMu     <- h2MuMu     as inp
    gamH2WW       <- h2WW       as inp
    gamH2ZZ       <- h2ZZ       as inp
    gamH2GaGa     <- h2GaGa     as inp
    gamH2GG       <- h2GG       as inp
    gamH2hh       <- h2hh       as inp
    gamH2HpHm     <- h2HpHm     as inp
    gamH2HpWm     <- h2HpWm     as inp
    gamH2HpWmStar <- h2HpWmStar as inp

    let gamH2HW     = 2 * gamH2HpWm  -- BR(H --> H+ W-) + BR(H --> H- W+)
        gamH2HWstar = 2 * gamH2HpWmStar

        totalWidth = sum [ gamH2TT, gamH2BB, gamH2CC , gamH2TauTau, gamH2MuMu
                         , gamH2WW, gamH2ZZ, gamH2GaGa, gamH2GG
                         , gamH2hh, gamH2HpHm
                         , gamH2HW, gamH2HWstar
                         ]

    return $ if totalWidth <= 0  -- what happend?
             then Nothing
             else Just $ BRH2 { _totalWidth = totalWidth
                              , _h2TT       = gamH2TT     / totalWidth
                              , _h2BB       = gamH2BB     / totalWidth
                              , _h2CC       = gamH2CC     / totalWidth
                              , _h2TauTau   = gamH2TauTau / totalWidth
                              , _h2MuMu     = gamH2MuMu   / totalWidth
                              , _h2WW       = gamH2WW     / totalWidth
                              , _h2ZZ       = gamH2ZZ     / totalWidth
                              , _h2GaGa     = gamH2GaGa   / totalWidth
                              , _h2GG       = gamH2GG     / totalWidth
                              , _h2hh       = gamH2hh     / totalWidth
                              , _h2HpHm     = gamH2HpHm   / totalWidth
                              , _h2HW       = gamH2HW     / totalWidth
                              , _h2HWstar   = gamH2HWstar / totalWidth
                              }
