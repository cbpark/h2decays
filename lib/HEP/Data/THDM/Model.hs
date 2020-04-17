{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module HEP.Data.THDM.Model
    (
      InputParam (..)
    , defaultM12
    , paramToArgsH2
    , paramToArgsHp
    , renderInputParamH2
    , renderInputParamHp

    , DecayWidth
    ) where

import HEP.Data.AlphaS                   (AlphaS)
import HEP.Data.Constants                (sqrt2)
import HEP.Data.Kinematics               (Mass (..))
import HEP.Data.THDM.Coupling            (THDMType (..))
import HEP.Data.Util

import Data.ByteString.Builder
import Data.Double.Conversion.ByteString (toFixed)
import Data.Hashable                     (Hashable)

import GHC.Generics                      (Generic)

data InputParam = InputParam { _mdtyp :: THDMType
                             , _mH    :: Mass
                             , _mA    :: Mass
                             , _mHp   :: Mass
                             , _m12   :: Mass
                             , _angs  :: Angles
                             } deriving (Generic, Show)

instance Hashable InputParam

-- |
--   m12^2 = 2 mbar^2 / sin(2 beta) = 2 m_H^2 / sin(2 beta)
--   seems to be a sane default (due to unitarity?).
defaultM12 :: Double -> Double -> Double
defaultM12 tanb mH
    | mH < 0 || tanb < 0 = 0
    | otherwise          = let sin2b = sin2Beta (mkAngles tanb 0)
                           in sqrt2 * mH / sqrt sin2b

paramToArgsH2 :: InputParam -> [String]
paramToArgsH2 InputParam {..} = let tanb  = tanBeta      _angs
                                    cosba = cosBetaAlpha _angs
                                in [ "--mtype", show _mdtyp
                                   , "--mH",    show _mH
                                   , "--mA",    show _mA
                                   , "--mHp",   show _mHp
                                   , "--m12",   show _m12
                                   , "--tanb",  show tanb
                                   , "--cosba", show cosba ]

paramToArgsHp :: InputParam -> [String]
paramToArgsHp InputParam {..} = let tanb  = tanBeta      _angs
                                    cosba = cosBetaAlpha _angs
                                in [ "--mtype", show _mdtyp
                                   , "--mHp",   show _mHp
                                   , "--tanb",  show tanb
                                   , "--cosba", show cosba ]

renderInputParamH2 :: InputParam -> Builder
renderInputParamH2 InputParam {..} =
    renderTHDMType _mdtyp
    <> space <> renderMass _mH
    <> space <> renderMass _mA
    <> space <> renderMass _mHp
    <> space <> renderMass _m12
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
