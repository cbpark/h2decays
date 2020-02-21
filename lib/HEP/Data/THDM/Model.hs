{-# LANGUAGE RecordWildCards #-}

module HEP.Data.THDM.Model
    (
      InputParam (..)
    , renderInputParamH2
    , renderInputParamHp
    , DecayWidth
    ) where

import HEP.Data.AlphaS                   (AlphaS)
import HEP.Data.Kinematics               (Mass (..))
import HEP.Data.THDM.Coupling            (THDMType (..))
import HEP.Data.Util                     (Angles, cosBetaAlpha, tanBeta)

import Data.ByteString.Builder
import Data.Double.Conversion.ByteString (toFixed)

data InputParam = InputParam { _mdtyp :: THDMType
                             , _mS    :: Mass
                             , _mH    :: Mass
                             , _mA    :: Mass
                             , _mHp   :: Mass
                             , _angs  :: Angles
                             } deriving Show

renderInputParamH2 :: InputParam -> Builder
renderInputParamH2 InputParam {..} =
    renderTHDMType _mdtyp
    <> space <> renderMass _mS
    <> space <> renderMass _mH
    <> space <> renderMass _mA
    <> space <> renderMass _mHp
    <> space <> renderAngles _angs

renderInputParamHp :: InputParam -> Builder
renderInputParamHp InputParam {..} =
    renderTHDMType _mdtyp
    <> space <> renderMass _mHp
    <> space <> renderAngles _angs

renderTHDMType :: THDMType -> Builder
renderTHDMType typ = let typ' | typ == TypeI  = 1
                              | typ == TypeII = 2
                              | otherwise     = 0
                     in int8Dec typ'

renderAngles :: Angles -> Builder
renderAngles angs =
    let tanb  = tanBeta angs
        cosba = cosBetaAlpha angs
    in (byteString . toFixed 1) tanb <> space <> (byteString . toFixed 2) cosba

renderMass :: Mass -> Builder
renderMass (Mass m) = (byteString . toFixed 2) m

space :: Builder
space = stringUtf8 " "

type DecayWidth m = AlphaS -> InputParam -> m Double
