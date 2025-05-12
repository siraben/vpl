{ mkDerivation, base, parsec, gloss, stdenv }:

mkDerivation {
  pname = "vpl";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base parsec gloss ];
  license = "mit";
  hydraPlatforms = stdenv.lib.platforms.none;
}
