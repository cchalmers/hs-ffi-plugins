{ sources ? import nix/sources.nix {} }:

let rust-channel-overlay = import sources.nixpkgs-mozilla;
    rust-overlay = self: super:
      let rust-stable = super.rustChannelOf {
            channel = "1.59.0";
            sha256 = "sha256:0dbar9p8spldj16zy7vahg9dq31vlkbrp40vq5f1q167cmjik1g0";
          };
          rust-nightly = super.rustChannelOf {
            channel = "nightly";
            date = "2020-11-03";
            sha256 = "1w5mwfix9d3xvf5hllj3ahiysf22f17ml1m0bqhaz4ky1xchhb2a";
          };
          rust-channel = rust-stable;
      in {
        rustc = rust-channel.rust;
        cargo = rust-channel.cargo;
        rustPlatform = self.makeRustPlatform { rustc = self.rustc; cargo = self.cargo; };
        rustfmt = rust-channel.rustfmt-preview;
        crate2nix =  self.callPackage sources.crate2nix {};
      };

    nixpkgs = import sources.nixpkgs {
      overlays = [ rust-channel-overlay rust-overlay ];
    };
    haskellPackages = nixpkgs.haskell.packages.ghc865.override {
      overrides = self: super: {
        # plugins = nixpkgs.haskell.lib.markUnbroken super.plugins;
        dyn2 = haskellPackages.callCabal2nix "dyn2" (filterHaskellSource ./dyn2) {};
        plugs = haskellPackages.callCabal2nix "plugs" (filterHaskellSource ./plugs) {};
        # plugins = haskellPackages.callCabal2nix "plugins" (filterHaskellSource ../plugins) {};
      };
    };

    hs-lib-hs =
      (haskellPackages.callCabal2nix "plugins_export" (filterHaskellSource ./hs) {}).overrideAttrs
      (old: {
        # don't know of a portable way to get the stub file, maybe a custom Setup.hs?
        postFixup = ''
          mkdir -p $out/include/plugins-hs
          cp dist/build/System/Plugins/Export_stub.h $out/include/plugins-hs/plugins-hs-export.h
          '';
      });

    ghc = haskellPackages.ghc;
    ghc-v = "${ghc.targetPrefix}ghc-${ghc.version}";

    # Copy out the parts of the library we care about. Also copy ghc's include files.
    hs-lib = nixpkgs.runCommandCC "plugins-export" {} ''
      mkdir -p $out/lib $out/include/plugins-hs
      cp ${hs-lib-hs}/include/plugins-hs/plugins-hs-export.h $out/include/plugins-hs/plugins-hs-export.h
      cp ${hs-lib-hs.src}/csrc/plugins-hs.h $out/include/plugins-hs/

      ${if nixpkgs.stdenv.isDarwin then
      ''
      cp ${hs-lib-hs}/lib/${ghc-v}/libplugins-export.* $out/lib/libplugins-export.dylib
      chmod +w $out/lib/*
      install_name_tool -id $out/lib/libplugins-export.dylib $out/lib/libplugins-export.dylib
      ''
      else
      ''
      cp ${hs-lib-hs}/lib/${ghc-v}/libplugins-export.* $out/lib/
      ''
      }
      cp -r ${ghc}/lib/${ghc-v}/include/* $out/include
      '';

    lib = nixpkgs.lib;
    filterHaskellSource = src:
      builtins.filterSource (path: type:
        lib.all (i: i != baseNameOf path)
        [ ".git" "dist-newstyle" "cabal.project.local"
          "dist" ".stack-work" ".DS_Store" "default.nix" "result"
        ]
          && lib.all (i: !(lib.hasSuffix i path)) [ ".lkshf" ]
          && lib.all
              (i: !(lib.hasPrefix i (baseNameOf path)))
              [ "cabal.project.local" ".ghc.environment." ]
        ) src;

    crateOverrides = nixpkgs.defaultCrateOverrides // {
      callback-rs = old: {
        LIBCLANG_PATH = "${nixpkgs.llvmPackages.libclang}/lib";
        buildInputs = old.buildInputs or [] ++ [ hs-lib ];
        nativeBuildInputs = [ nixpkgs.clang ];
      };
    };

    crateNix = nixpkgs.callPackage ./rs/Cargo.nix {
      defaultCrateOverrides = crateOverrides;
    };

    # The packages of this workspace with our crateOverrides
    packages = lib.mapAttrs (_: pkg: pkg.build) crateNix.workspaceMembers;

    example-env = haskellPackages.ghcWithPackages (p: with p; [ lens dyn2 ]);

in  { inherit (haskellPackages) dyn2 plugs; inherit nixpkgs hs-lib hs-lib-hs example-env; } // packages
