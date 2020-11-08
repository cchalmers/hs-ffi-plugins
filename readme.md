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
$ cabal watch
```
