#include "alphas.h"
#include "LHAPDF/LHAPDF.h"

extern "C" {
CAlphaS *mkAlphaS(double mt, double mz, double alpha) {
    // See https://lhapdf.hepforge.org/_2tests_2testalphas_8cc-example.html
    LHAPDF::AlphaS *as = new LHAPDF::AlphaS_ODE();
    as->setOrderQCD(5);
    as->setMZ(mz);
    as->setAlphaSMZ(alpha);
    as->setQuarkMass(4, 1.27);
    as->setQuarkMass(5, 4.18);  // without this, alpha_s will blow up.
    as->setQuarkMass(6, mt);

    return reinterpret_cast<CAlphaS *>(as);
}

double alphasQ(CAlphaS *as_c, double q) {
    LHAPDF::AlphaS *as = reinterpret_cast<LHAPDF::AlphaS *>(as_c);
    return as->alphasQ(q);
}
}
