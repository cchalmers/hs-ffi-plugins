use callback_rs::dynamic::Typeable;
use callback_rs::ffi;
use callback_rs::list::HsList;
use callback_rs::session;
// use callback_rs::list::HsList;

fn main() {
    unsafe {
        ffi::potatoInit();
    }

    let session = session::Session::new(Some("/nix/store/f7fn5bichmzpn39bglwsiijv569k4smr-ghc-8.6.5-with-packages/lib/ghc-8.6.5"));
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
    // from ghci: 
// Prelude GHC System.Plugins.Export> map fst <$> ps
// Just ["/nix/store/f7fn5bichmzpn39bglwsiijv569k4smr-ghc-8.6.5-with-packages/lib/ghc-8.6.5/package.conf.d"]

    // from rust:
    // the package database is Just
    // ["/nix/store/wrghpsajnhd55blll01zw5wiw5vwar84-ghc-8.6.5/lib/ghc-8.6.5/package.conf.d"]

    session.import_modules(&["Prelude", "Plug2"]);
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

    // let ptr = unsafe { dynamic::load_symbol("../dyn/Plug.so", "myCoolList") };
    // let list = unsafe { HsList::from_ptr(ptr) };
    // eprintln!("made the list");
    // list.for_each(|x| eprintln!("x = {}", x));
    // let ptr = unsafe { dynamic::load_symbol("../dyn2/Plug2.o", "myCoolList") };
    // let list = unsafe { HsList::from_ptr(ptr) };
    // list.for_each(|x| eprintln!("y = {}", x));
}
