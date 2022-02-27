let default = import ./.. {};
    pkgs = default.nixpkgs;
in  default.callback-rs.overrideAttrs (old: {
      buildInputs = with pkgs; old.buildInputs or [] ++ [
        rustc
        cargo
        cargo-edit
        crate2nix
      ];
      MY_GHCLIB = "${default.example-env}/lib/ghc-8.6.5";
    })
