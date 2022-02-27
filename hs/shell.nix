{ default ? import ./.. {} }:

let pkgs = default.nixpkgs;
    haskellPackages = pkgs.haskellPackages;
in  default.plugs.env.overrideAttrs (attrs: {
  buildInputs = attrs.buildInputs or [] ++ [
    haskellPackages.ghc
    haskellPackages.cabal-install
    haskellPackages.ghcid
  ];
})
