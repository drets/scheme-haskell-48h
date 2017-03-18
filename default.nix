with import <nixpkgs> { };

haskell.lib.buildStackProject {
  name = "scheme";
  ghc = haskellPackages.ghc;
  buildInputs = [ git
                  cabal-install
                  zlib

                  haskellPackages.parsec
                  haskellPackages.HUnit
                  haskellPackages.vector ];
}
