use callback_rs::dynamic;
use callback_rs::ffi;
use callback_rs::list::HsList;

fn main() {
    unsafe {
        ffi::potatoInit();
    }

    let ptr = unsafe { dynamic::load_symbol("../dyn/Plug.o", "myCoolList") };
    let list = unsafe { HsList::from_ptr(ptr) };
    list.for_each(|x| eprintln!("x = {}", x));
}
