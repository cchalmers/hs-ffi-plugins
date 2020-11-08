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

    Ok(())
}
