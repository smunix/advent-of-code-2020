{ mkDerivation, base, hpack, hspec, optparse-simple, rio, stdenv, version ? "unknown" }:
mkDerivation {
  inherit version;
  pname = "d1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base rio ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base optparse-simple rio ];
  testHaskellDepends = [ base hspec rio ];
  doHaddock = false;
  doCheck = false;
  prePatch = "hpack";
  homepage = "https://github.com/smunix/d1#readme";
  license = stdenv.lib.licenses.bsd3;
}
