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
    println!("cargo:rerun-if-env-changed=NIX_CFLAGS_COMPILE");
}
