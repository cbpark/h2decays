{ mkDerivation, base, bytestring, double-conversion, gsl, lhapdf, optparse-generic, stdenv
}:
mkDerivation {
  pname = "h2decays";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring double-conversion optparse-generic
  ];
  executableSystemDepends = [ gsl lhapdf ];
  homepage = "https://github.com/cbpark/h2decays";
  description = "Calculating decay widths and branching ratios of the heavy Higgs boson in the 2HDM";
  license = stdenv.lib.licenses.gpl3;
}
