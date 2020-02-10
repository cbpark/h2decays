{-# LANGUAGE RecordWildCards #-}

module HEP.Data.THDM.Model
    (
      InputParam (..)
    , renderInputParam
    , DecayWidth
    ) where

import HEP.Data.AlphaS                   (AlphaS)
import HEP.Data.Kinematics               (Mass (..))
import HEP.Data.THDM.Coupling            (THDMType (..))
import HEP.Data.Util                     (Angles, cosBetaAlpha, tanBeta)

import Data.ByteString.Builder
import Data.Double.Conversion.ByteString (toFixed)

data InputParam =
    InputParam { _mdtyp :: THDMType
               , _mH    :: Mass
               , _mA    :: Mass
               , _mHp   :: Mass
               , _angs  :: Angles
               } deriving Show

renderInputParam :: InputParam -> Builder
renderInputParam InputParam {..} =
    let typ | _mdtyp == TypeI  = 1
            | _mdtyp == TypeII = 2
            | otherwise       = 0
        tanb = tanBeta _angs
        cosba = cosBetaAlpha _angs
    in int8Dec typ
       <> space <> (byteString . toFixed 1) (getMass _mH)
       <> space <> (byteString . toFixed 1) (getMass _mA)
       <> space <> (byteString . toFixed 1) (getMass _mHp)
       <> space <> (byteString . toFixed 2) tanb
       <> space <> (byteString . toFixed 2) cosba
  where
    space = stringUtf8 "  "

type DecayWidth m = AlphaS -> InputParam -> m Double
