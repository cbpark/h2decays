module HEP.Data.Constants where

import HEP.Data.Kinematics (Mass (..), massSq)

mW, mZ, mh :: Mass
mW = Mass 80.379
mZ = Mass 91.1876
mh = Mass 125.0

mW2, mZ2, mh2 :: Double
mW2 = massSq mW
mZ2 = massSq mZ
mh2 = massSq mh

-- | pole masses
mt, mb, mc, mtau :: Mass
mt   = Mass 173.0
mb   = Mass 4.78
mc   = Mass 1.67
mtau = Mass 1.777

mt2, mb2, mc2, mtau2 :: Double
mt2   = massSq mt
mb2   = massSq mb
mc2   = massSq mc
mtau2 = massSq mtau

gFermi, gW, gW2 :: Double
gFermi = 1.1663787e-5
gW2    = 8 * mW2 * gFermi / sqrt2
gW     = sqrt gW2

vEW, vEW2 :: Double
vEW2 = 2 * mW2 / gW2
vEW  = sqrt vEW2

sinThetaW2, alpha, alphasMZ :: Double
sinThetaW2 = 0.23122
alpha      = gW2 * sinThetaW2 / (4 * pi)
alphasMZ   = 0.118

vUD, vUS, vUB, vCD, vCS, vCB, vTD, vTS, vTB :: Double
vUD = 0.97446
vUS = 0.22452
vUB = 0.00365
vCD = 0.22438
vCS = 0.97359
vCB = 0.04214
vTD = 0.00896
vTS = 0.04133
vTB = 0.999105

sqrt2 :: Double
sqrt2 = 1.4142135623730951

pi2, pi3 :: Double
pi2 = 9.869604401089358
pi3 = 31.006276680299816
