with import <nixpkgs> {};
# packageOverrides = super: let self = super.pkgs; in
let myHaskellEnv = haskellPackages.ghcWithPackages
                     (haskellPackages: with haskellPackages; [
                       # libraries
                       random
                       repa
                       JuicyPixels-repa
                     ]);
in
stdenv.mkDerivation rec{
  name="haskell";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    myHaskellEnv
  ];
}
