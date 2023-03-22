with import <nixpkgs> {};

let
  ihaskell = haskellPackages.ihaskell;
  env = haskellPackages.shellFor {
    packages = p: [p.ihaskell];
  };
in
  env
