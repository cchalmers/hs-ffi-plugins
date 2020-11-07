use callback_rs::session;
use callback_rs::ffi;
// use callback_rs::list::HsList;

fn main() {
    unsafe {
        ffi::potatoInit();
    }

    let session = session::Session::new();
    session.import_module("Prelude");
    session.run_expr("head [1,2,3]");

    // let ptr = unsafe { dynamic::load_symbol("../dyn/Plug.so", "myCoolList") };
    // let list = unsafe { HsList::from_ptr(ptr) };
    // eprintln!("made the list");
    // list.for_each(|x| eprintln!("x = {}", x));
    // let ptr = unsafe { dynamic::load_symbol("../dyn2/Plug2.o", "myCoolList") };
    // let list = unsafe { HsList::from_ptr(ptr) };
    // list.for_each(|x| eprintln!("y = {}", x));
}
