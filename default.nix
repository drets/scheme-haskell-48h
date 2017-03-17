with import <nixpkgs> { };

haskell.lib.buildStackProject {
  name = "scheme";
  ghc = haskellPackages.ghc;
  buildInputs = [ git
                  cabal-install
                  zlib
                  cairo
                  haskellPackages.parsec
                  haskellPackages.HUnit ];
}
