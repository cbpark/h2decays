module HEP.Data.THDM.Parser (parseBRH2, parseBRH2') where

import           HEP.Data.Kinematics              (Mass (..))
import           HEP.Data.THDM.BranchingRatio     (BRH2 (..))
import           HEP.Data.THDM.Coupling           (THDMType (..))
import           HEP.Data.THDM.Model              (InputParam (..))
import           HEP.Data.Util                    (mkAngles)

import           Control.Monad.Trans.State.Strict (StateT (..))
import           Data.Attoparsec.ByteString       (skipWhile)
import           Data.Attoparsec.ByteString.Char8 hiding (skipWhile)
import           Data.ByteString.Char8            (ByteString)
import           Pipes
import qualified Pipes.Attoparsec                 as PA

import           Control.Applicative              ((<|>))

parseBRH2 :: Monad m
          => Producer ByteString m () -> Producer (InputParam, BRH2) m ()
parseBRH2 s = do (r, s') <- lift $ runStateT (PA.parse parseBRH2') s
                 case r of
                     Just (Right br) -> yield br >> parseBRH2 s'
                     _               -> return ()

parseBRH2' :: Parser (InputParam, BRH2)
parseBRH2' = do
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
    mS     <- double <* skipSpace
    mH     <- double <* skipSpace
    mA     <- double <* skipSpace
    mHp    <- double <* skipSpace
    tanb   <- double <* skipSpace
    cosba  <- double

    let mdtyp | mdtypV == '1' = TypeI
              | mdtypV == '2' = TypeII
              | otherwise   = UnknownType

    return $ InputParam { _mdtyp = mdtyp
                        , _mS    = Mass mS
                        , _mH    = Mass mH
                        , _mA    = Mass mA
                        , _mHp   = Mass mHp
                        , _angs  = mkAngles tanb cosba }

skipComment :: Parser ()
skipComment = void $ many' ((char '#' <|> char '-') >> skipTillEnd)
  where skipTillEnd = skipWhile (not . isEndOfLine) >> endOfLine
