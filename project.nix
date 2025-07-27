{ mkDerivation, base, containers, parsec, gloss, pretty, effectful, effectful-core, stdenv }:

mkDerivation {
  pname = "vpl";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers parsec gloss pretty effectful effectful-core ];
  license = "mit";
  hydraPlatforms = stdenv.lib.platforms.none;
}