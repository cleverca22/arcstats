{ haskellPackages, haskell, stdenv, lib }:

let
  overlay = self: super: {
    datadog = haskell.lib.dontCheck (super.callHackage "datadog" "0.2.4.0" {});
    buffer-builder = haskell.lib.dontCheck (super.callHackage "buffer-builder" "0.2.4.6" {});
  };
  ghc = (haskellPackages.extend overlay).ghcWithPackages (ps: with ps; [ datadog lens data-default ]);
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
