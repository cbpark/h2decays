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
    InputParam { mdtyp :: THDMType
               , mH    :: Mass
               , mA    :: Mass
               , mHp   :: Mass
               , angs  :: Angles
               } deriving Show

renderInputParam :: InputParam -> Builder
renderInputParam InputParam {..} =
    let typ | mdtyp == TypeI  = 1
            | mdtyp == TypeII = 2
            | otherwise       = 0
        tanb = tanBeta angs
        cosba = cosBetaAlpha angs
    in int8Dec typ
       <> space <> (byteString . toFixed 1) (getMass mH)
       <> space <> (byteString . toFixed 1) (getMass mA)
       <> space <> (byteString . toFixed 1) (getMass mHp)
       <> space <> (byteString . toFixed 2) tanb
       <> space <> (byteString . toFixed 2) cosba
  where
    space = stringUtf8 "  "

type DecayWidth m = AlphaS -> InputParam -> m Double
