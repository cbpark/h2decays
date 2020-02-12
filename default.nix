{ mkDerivation, base, blaze-builder, bytestring, double-conversion
, gsl, hmatrix-gsl, lhapdf, optparse-generic, pipes , pipes-bytestring
, stdenv, vector
}:
mkDerivation {
  pname = "h2decays";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base blaze-builder bytestring double-conversion hmatrix-gsl
    optparse-generic pipes pipes-bytestring vector
  ];
  executableSystemDepends = [ gsl lhapdf ];
  homepage = "https://github.com/cbpark/h2decays";
  description = "Calculating decay widths and branching ratios of the heavy Higgs boson in the 2HDM";
  license = stdenv.lib.licenses.gpl3;
}
