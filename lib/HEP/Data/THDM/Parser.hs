module HEP.Data.THDM.Parser (parseBRH2) where

import HEP.Data.Kinematics              (Mass (..))
import HEP.Data.THDM.BranchingRatio     (BRH2 (..))
import HEP.Data.THDM.Coupling           (THDMType (..))
import HEP.Data.THDM.Model              (InputParam (..))
import HEP.Data.Util                    (mkAngles)

import Data.Attoparsec.ByteString       (skipWhile)
import Data.Attoparsec.ByteString.Char8 hiding (skipWhile)

import Control.Applicative              ((<|>))
import Control.Monad                    (void)

parseBRH2 :: Parser (InputParam, BRH2)
parseBRH2 = do
    skipComment >> skipSpace
    inp <- parseInputParamH2 <* skipSpace
    totalWidth <- double <* skipSpace
    h2TT       <- double <* skipSpace
    h2BB       <- double <* skipSpace
    h2CC       <- double <* skipSpace
    h2TauTau   <- double <* skipSpace
    h2MuMu     <- double <* skipSpace
    h2WW       <- double <* skipSpace
    h2ZZ       <- double <* skipSpace
    h2GaGa     <- double <* skipSpace
    h2GG       <- double <* skipSpace
    h2hh       <- double <* skipSpace
    h2HpHm     <- double <* skipSpace
    h2HpmW     <- double <* skipSpace
    h2AZ       <- double <* endOfLine

    let br = BRH2 { _totalWidth = totalWidth
                  , _h2TT       = h2TT
                  , _h2BB       = h2BB
                  , _h2CC       = h2CC
                  , _h2TauTau   = h2TauTau
                  , _h2MuMu     = h2MuMu
                  , _h2WW       = h2WW
                  , _h2ZZ       = h2ZZ
                  , _h2GaGa     = h2GaGa
                  , _h2GG       = h2GG
                  , _h2hh       = h2hh
                  , _h2HpHm     = h2HpHm
                  , _h2HpmW     = h2HpmW
                  , _h2AZ       = h2AZ }
    return (inp, br)

parseInputParamH2 :: Parser InputParam
parseInputParamH2 = do
    skipSpace
    mdtypV <- digit  <* skipSpace
    mH     <- double <* skipSpace
    mA     <- double <* skipSpace
    mHp    <- double <* skipSpace
    m12    <- double <* skipSpace
    tanb   <- double <* skipSpace
    cosba  <- double

    let mdtyp | mdtypV == '1' = TypeI
              | mdtypV == '2' = TypeII
              | otherwise   = UnknownType

    return $ InputParam { _mdtyp = mdtyp
                        , _mH    = Mass mH
                        , _mA    = Mass mA
                        , _mHp   = Mass mHp
                        , _m12   = Mass m12
                        , _angs  = mkAngles tanb cosba }

skipComment :: Parser ()
skipComment = void $ many' ((char '#' <|> char '-') >> skipTillEnd)
  where skipTillEnd = skipWhile (not . isEndOfLine) >> endOfLine
