{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module HEP.Data.THDM.BranchingRatio (getBRH2, getBRHp) where

import HEP.Data.AlphaS                   (AlphaS)
import HEP.Data.THDM.DecayWidth
import HEP.Data.THDM.Model               (InputParam, renderInputParam)

import Data.ByteString.Builder
import Data.Double.Conversion.ByteString (toExponential, toFixed)

import Control.Monad.IO.Class            (MonadIO)

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
                 }

getBRH2 :: MonadIO m => AlphaS -> InputParam -> m Builder
getBRH2 as inp = renderBRH2 inp <$> brH2 as inp

renderBRH2 :: InputParam -> Maybe BRH2 -> Builder
renderBRH2 inp brh2 = renderInputParam inp <> space <> renderBRH2' brh2
  where
    renderBRH2' :: Maybe BRH2 -> Builder
    renderBRH2' Nothing          =
        (byteString . toFixed 4) 0
        <> foldr1 (<>) (replicate 13 (space <> convDbl 0))
        <> endLine
    renderBRH2' (Just BRH2 {..}) =
        (byteString . toFixed 4) _totalWidth
        <> space <> convDbl _h2TT
        <> space <> convDbl _h2BB
        <> space <> convDbl _h2CC
        <> space <> convDbl _h2TauTau
        <> space <> convDbl _h2MuMu
        <> space <> convDbl _h2WW
        <> space <> convDbl _h2ZZ
        <> space <> convDbl _h2GaGa
        <> space <> convDbl _h2GG
        <> space <> convDbl _h2hh
        <> space <> convDbl _h2HpHm
        <> space <> convDbl _h2HW
        <> space <> convDbl _h2HWstar
        <> endLine

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

    return $ if totalWidth <= 0  -- what happened?
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

data BRHp = BRHp { _totalWidth :: Double
                 , _hpTB       :: Double
                 , _hpCS       :: Double
                 , _hpTauNu    :: Double
                 , _hpMuNu     :: Double
                 , _hpWh       :: Double
                 }

getBRHp :: MonadIO m => AlphaS -> InputParam -> m Builder
getBRHp as inp = renderBRHp inp <$> brHp as inp

renderBRHp :: InputParam -> Maybe BRHp -> Builder
renderBRHp inp brhp = renderInputParam inp <> space <> renderBRHp' brhp
  where
    renderBRHp' :: Maybe BRHp -> Builder
    renderBRHp' Nothing          =
        (byteString . toFixed 4) 0
        <> foldr1 (<>) (replicate 5 (space <> convDbl 0))
        <> endLine
    renderBRHp' (Just BRHp {..}) =
        (byteString . toFixed 4) _totalWidth
        <> space <> convDbl _hpTB
        <> space <> convDbl _hpCS
        <> space <> convDbl _hpTauNu
        <> space <> convDbl _hpMuNu
        <> space <> convDbl _hpWh
        <> endLine

brHp :: MonadIO m => AlphaS -> InputParam -> m (Maybe BRHp)
brHp as inp = do
    gamHpTB    <- hpTB as inp
    gamHpCS    <- hpCS as inp
    gamHpTauNu <- hpTauNu as inp
    gamHpMuNu  <- hpMuNu as inp
    gamHpWh    <- hpWh as inp

    let totalWidth = sum [ gamHpTB, gamHpCS, gamHpTauNu, gamHpMuNu, gamHpWh ]
    return $ if totalWidth <= 0
             then Nothing
             else Just $ BRHp { _totalWidth = totalWidth
                              , _hpTB       = gamHpTB    / totalWidth
                              , _hpCS       = gamHpCS    / totalWidth
                              , _hpTauNu    = gamHpTauNu / totalWidth
                              , _hpMuNu     = gamHpMuNu  / totalWidth
                              , _hpWh       = gamHpWh    / totalWidth
                              }

convDbl :: Double -> Builder
convDbl = byteString . toExponential 5

space :: Builder
space = stringUtf8 "  "

endLine :: Builder
endLine = charUtf8 '\n'
