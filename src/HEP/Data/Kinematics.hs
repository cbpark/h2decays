{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HEP.Data.Kinematics where

newtype Mass = Mass { getMass :: Double } deriving (Eq, Ord, Num, Show)

massSq :: Mass -> Double
massSq (Mass m) = m * m

massRatio :: Mass -> Mass -> Double
massRatio (Mass m1) (Mass m2) = m1 / m2

betaF :: Mass  -- ^ heavy particle
      -> Mass  -- ^ light particle
      -> Double
betaF mY mF = if a' < b then 0 else sqrt (1 - b / a)
  where (a, b) = (massSq mY, 4 * massSq mF)
        a' = a + 1.0e-8
