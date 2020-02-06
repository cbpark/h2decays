module HEP.Data.THDM.DecayWidth.H2 where

import HEP.Data.Constants     (mtau)
import HEP.Data.Kinematics    (Mass (..), betaF)
import HEP.Data.THDM.Coupling

gammaTauTauH2 :: THDMType -> Mass -> (Double, Double) -> Double
gammaTauTauH2 typ m@(Mass mH) angles =
    let gH | typ == TypeI  = gHTauTauI  angles
           | typ == TypeII = gHTauTauII angles
           | otherwise     = 0

        beta = betaF m mtau
    in mH * gH * gH * beta ** 3 / (32 * pi)
