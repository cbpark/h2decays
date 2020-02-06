{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HEP.Data.Kinematics where

newtype Mass = Mass Double deriving (Eq, Ord, Num, Show)

massSq :: Mass -> Double
massSq (Mass m) = m * m

massRatio :: Mass -> Mass -> Double
massRatio (Mass m1) (Mass m2) = m1 / m2
