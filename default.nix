{ mkDerivation, attoparsec, base, blaze-builder, bytestring
, double-conversion, gsl, hmatrix-gsl, hpack, lhapdf
, optparse-generic, pipes, pipes-attoparsec, stdenv, transformers
, vector
}:
mkDerivation {
  pname = "h2decays";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base blaze-builder bytestring double-conversion
    hmatrix-gsl pipes pipes-attoparsec transformers vector
  ];
  librarySystemDepends = [ gsl lhapdf ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring optparse-generic pipes vector
  ];
  prePatch = "hpack";
  homepage = "https://github.com/cbpark/h2decays#readme";
  description = "Calculating the branching ratios of heavy Higgs bosons in the 2HDM";
  license = stdenv.lib.licenses.gpl3;
}
