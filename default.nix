{ haskellPackages, haskell, stdenv, lib }:

let
  ghc = haskellPackages.ghcWithPackages (ps: with ps; [ (haskell.lib.dontCheck (ps.callHackage "datadog" "0.2.4.0" {})) lens data-default ]);
in stdenv.mkDerivation {
  name = "arcstats";
  buildInputs = [ ghc haskellPackages.ghcid ];
  src = lib.cleanSource ./.;
  buildPhase = ''
    mkdir $out/bin -p
    ghc src/Main.hs -o $out/bin/arcstats
  '';
  dontInstall = true;
}
