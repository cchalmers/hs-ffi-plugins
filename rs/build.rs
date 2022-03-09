use std::env;
use std::path::PathBuf;

fn main() {
    // A cargo directory used for storing intermediate files for build scripts.
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());

    let bindings = bindgen::Builder::default()
        .header_contents(
            "plugins.h",
            "#include <plugins-hs/plugins-hs.h>\n#include <plugins-hs/plugins-hs-export.h>",
        )
        .derive_default(true)
        .generate()
        .expect("Unable to generate callback.h bindings");

    bindings
        .write_to_file(out_path.join("hs.rs"))
        .expect("Failed to write bindings!");

    println!("cargo:rustc-link-lib=plugins-export");
    println!("cargo:rustc-link-lib=HSrts-ghc8.6.5");
    // this is needed if I want to use things like hs_perform_gc
    println!("cargo:rustc-link-search=/nix/store/vm0pj1wsk434fv5hlds1ndyfa9x4bb7l-ghc-8.6.5/lib/ghc-8.6.5/rts");
    println!("cargo:rerun-if-env-changed=NIX_CFLAGS_COMPILE");
}
