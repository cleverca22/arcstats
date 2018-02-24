with import <nixpkgs> {};

let
  ghc = haskellPackages.ghcWithPackages (ps: with ps; [ (haskell.lib.dontCheck datadog) lens ]);
in stdenv.mkDerivation {
  name = "arcstats";
  buildInputs = [ ghc ];
  src = lib.cleanSource ./.;
  buildPhase = ''
    mkdir $out/bin -p
    ghc src/Main.hs -o $out/bin/arcstats
  '';
  dontInstall = true;
}
