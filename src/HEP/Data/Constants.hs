module HEP.Data.Constants where

type Mass = Double

mW, mZ, mh :: Mass
mW = 80.379
mZ = 91.1876
mh = 125.0

mW2, mZ2, mh2 :: Double
mW2 = mW * mW
mZ2 = mZ * mZ
mh2 = mh * mh

-- | pole masses
mt, mb, mc, mtau :: Mass
mt   = 173.0
mb   = 4.78
mc   = 1.67
mtau = 1.777

mt2, mb2, mc2, mtau2 :: Double
mt2   = mt * mt
mb2   = mb * mb
mc2   = mc * mc
mtau2 = mtau * mtau

gFermi, gW, gW2 :: Double
gFermi = 1.1663787e-5
gW2    = 8 * mW2 * gFermi / sqrt2
gW     = sqrt gW2

vEW, vEW2 :: Double
vEW2 = 2 * mW2 / gW2
vEW  = sqrt vEW

sinThetaW2, alpha, alphasMZ :: Double
sinThetaW2 = 0.23122
alpha      = gW2 * sinThetaW2 / (4 * pi)
alphasMZ   = 0.1179

sqrt2 :: Double
sqrt2 = 1.4142135623730951
