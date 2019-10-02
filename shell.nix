{ pkgs ? <nixpkgs> }:

# nixos 19-03

with import pkgs {};

let
  hsPkgs0 = haskellPackages.override {
    overrides = hself: hsuper:
      {
        semirings = hself.callPackage ./overrides/semirings.nix {};
      };
  }; # haskellPackages override
  hsPkgs = hsPkgs0.extend (haskell.lib.packageSourceOverrides {
        GrammarProducts =  ./.;
        #
        ADPfusion = ./deps/ADPfusion;
        FormalGrammars =  ./deps/FormalGrammars;
        PrimitiveArray =  ./deps/PrimitiveArray;
        #
        DPutils =  ./deps/DPutils;
        OrderedBits =  ./deps/OrderedBits;
        #
  }); # extend
  # my own little tool
  cabalghcisrc =
    let local = ~/Documents/University/devel/ghcicabal;
    in  if builtins.pathExists local
        then local
        else builtins.fetchGit {
          url = https://github.com/choener/ghcicabal;
          ref = "master";
        };
  cabalghci = hsPkgs.callPackage cabalghcisrc {};
in

hsPkgs.shellFor {
  packages = p: [
    p.ADPfusion
    p.DPutils
    p.FormalGrammars
    p.GrammarProducts
    p.OrderedBits
    p.PrimitiveArray
                ];
  withHoogle = true;
  buildInputs = [
    cabal-install
    llvm
    # haskellPackages.ghcid
    # haskellPackages.hpack
    cabalghci
    haskell-ci
    # hsPkgs.nvim-hs-ghcid
  ];
} # shellFor

