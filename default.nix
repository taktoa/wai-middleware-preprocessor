{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
let
  inherit (haskellPackages)
    cabal
    cabalInstall
    # Haskell dependencies here
    Cabal_1_22_0_0
    waiMiddlewareStatic
    mtl
    split
    text
    wai
    warp;

in cabal.mkDerivation (self: {
  pname = "wai-middleware-preprocessor";
  version = "0.2.0.0";
  src = ./.;
  buildDepends = [
    # Same as above
    Cabal_1_22_0_0
    waiMiddlewareStatic
    mtl
    split
    text
    wai
    warp
  ];
  buildTools = [ cabalInstall ];
  enableSplitObjs = false;
})
