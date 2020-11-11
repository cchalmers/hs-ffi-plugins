use callback_rs::dynamic::Typeable;
use callback_rs::ffi;
use callback_rs::list::HsList;
use callback_rs::session;
use color_eyre::eyre::Result;

use std::path::PathBuf;
use structopt::StructOpt;

/// A basic example
#[derive(StructOpt, Debug)]
#[structopt(name = "basic")]
struct Opt {
    libdir: PathBuf,
}

fn main() -> Result<()> {
    unsafe {
        ffi::potatoInit();
    }

    let opt = Opt::from_args();

    let session = session::Session::new(Some(&opt.libdir));

    session.import_modules(&["Prelude"]);
    session.run_expr("head [1,2,3]");
    session.import_modules(&["Prelude", "Data.Word"]);
    if let Some(dynamic) = session.run_expr_dyn("head [4,1,2,3] :: Word64") {
        eprintln!("the value is {:?}", u64::from_dynamic(&dynamic))
    } else {
        eprintln!("head [4,1,2,3]");
    }

    if let Some(dynamic) = session.run_expr_dyn("[4,1,2,3] :: [Word64]") {
        eprintln!(
            "the value is {:?}",
            HsList::<u64>::from_dynamic(&dynamic)
                .expect("wasn't a list")
                .collect::<Vec<u64>>()
        )
    } else {
        eprintln!("Was not successful");
    }

    session.debugging();

    session.import_modules(&["Prelude", "Plug2", "Control.Lens", "Data.Word"]);
    if let Some(dynamic) = session.run_expr_dyn("myCoolList") {
        eprintln!(
            "the value is {:?}",
            HsList::<u64>::from_dynamic(&dynamic)
                .expect("wasn't a list")
                .collect::<Vec<u64>>()
        )
    } else {
        eprintln!("Was not successful");
    }

    if let Some(dynamic) = session.run_expr_dyn("[1,2,3] & each +~ 1 :: [Word64]") {
        eprintln!(
            "the value is {:?}",
            HsList::<u64>::from_dynamic(&dynamic)
                .expect("wasn't a list")
                .collect::<Vec<u64>>()
        )
    } else {
        eprintln!("Was not successful");
    }

    if !session.set_load_paths(&["examples/test.hs"]) {
        panic!("OH NO")
    }

    if let Some(dynamic) = session.run_expr_dyn("myCoolerList") {
        eprintln!(
            "the value is {:?}",
            HsList::<u64>::from_dynamic(&dynamic)
                .expect("wasn't a list")
                .collect::<Vec<u64>>()
        )
    } else {
        eprintln!("Was not successful");
    }

    session.set_import_paths(&["/home/chris/Documents/hs-ffi-plugins/interpret-me"]);
    // TODO work out how to load modules in include path without having to set a load path that
    // imports them
    if !session.set_load_paths(&["examples/include-stuff.hs"]) {
        panic!("OH NO")
    }
    session.import_modules(&["Prelude", "Blah", "Blah.Inner"]);

    if let Some(dynamic) = session.run_expr_dyn("blah") {
        eprintln!(
            "the value is {:?}",
            HsList::<u64>::from_dynamic(&dynamic)
                .expect("wasn't a list")
                .collect::<Vec<u64>>()
        )
    } else {
        eprintln!("Was not successful");
    }

    if let Some(dynamic) = session.run_expr_dyn("blahInner") {
        eprintln!(
            "the value is {:?}",
            HsList::<u64>::from_dynamic(&dynamic)
                .expect("wasn't a list")
                .collect::<Vec<u64>>()
        )
    } else {
        eprintln!("Was not successful");
    }

    Ok(())
}
