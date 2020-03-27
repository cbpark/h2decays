{ mkDerivation, base, blaze-builder, bytestring, double-conversion
, gsl, hmatrix-gsl, hpack, lhapdf, optparse-generic
, pipes, pipes-bytestring, stdenv, vector
}:
mkDerivation {
  pname = "h2decays";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base blaze-builder bytestring double-conversion hmatrix-gsl pipes
    pipes-bytestring vector
  ];
  librarySystemDepends = [ gsl lhapdf ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring optparse-generic pipes vector
  ];
  prePatch = "hpack";
  homepage = "https://github.com/cbpark/h2decays#readme";
  description = "Calculating decay widths and branching ratios of the heavy Higgs boson in the 2HDM";
  license = stdenv.lib.licenses.gpl3;
}
