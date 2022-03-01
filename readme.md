A framework for haskell ffi with rust. The main use case for now is calling Haskell from rust.

Usually Haskell ffi is done with `foreign export ccall` but this is unpleasent to work with. This
uses the GHC api to dynamically import normal Haskell libraries or directly interpret Haskell code.

Very much in pre-alpha state.

## ghc workflow

```
$ cd hs
$ nix-shell
$ ghcid -c "cabal repl --repl-options -fobject-code"
```

## rust workflow

```
$ cd rs
$ nix-shell
$ cargo watch
```

The repl is a good interactive way to test things out:
```
$ cargo run --example repl
```

You can also link the libplugins-export.so:
```
$ cabal build
$ export LD_LIBRARY_PATH=realpath $(dirname $(fd libplugins-export.))
$ cargo run --example repl
```

After that you just need to cabal build and reload the repl.

## Notes

I want to be able to load both compiled and interpreted haskell and be able to inspect what's
available and run code. These things seem to work:

  - building a nix shell with the required package as a dependency, importing with
    `::import_modules` and eval'ing
  - building with --write-ghc-environment=always and setting GHC_ENVIRONMENT to point to the
    .ghc.environment file (or one of the many of options, see DynFlags). Again, you can
    `Session::import_modules`
  - load with `Session::set_load_paths` (this can be used in combination with a nix-shell
      environment, a .ghc environment and set_import_paths)
