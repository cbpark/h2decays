{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "h2decays";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/cbpark/h2decays";
  license = stdenv.lib.licenses.gpl3;
}
