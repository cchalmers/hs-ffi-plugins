use callback_rs::ffi;
use callback_rs::list;
use callback_rs::bytes;

// fn main() {
//     unsafe {
//         ffi::potatoInit();
//         ffi::hello();
//         ffi::potatoExit();
//     }
// }

// use std::cell::Cell;
// use std::cell::UnsafeCell;
// use std::os::raw::*;
// use std::ptr;
// use std::sync::Arc;

// use std::sync::atomic::Ordering;
// use std::sync::atomic::{AtomicI32, AtomicUsize};

// struct HsList {
//     ptr: ffi::HsStablePtr,
// }

// impl HsList {
//     fn new() -> HsList {
//         HsList {
//             ptr: unsafe { ffi::mkNewList() },
//         }
//     }
// }

// impl Iterator for HsList {
//     type Item = u64;

//     fn next(&mut self) -> Option<u64> {
//         let mut x = 0;
//         let present = unsafe { ffi::nextList64(self.ptr, &mut x as *mut u64 as _) };
//         if present != 0 {
//             Some(x)
//         } else {
//             None
//         }
//     }
// }

// struct HsListDF {
//     ptr: ffi::HsStablePtr,
// }

// impl HsListDF {
//     fn new() -> HsListDF {
//         HsListDF {
//             ptr: unsafe { ffi::mkNewList() },
//         }
//     }
// }

// impl Iterator for HsListDF {
//     type Item = u64;

//     fn next(&mut self) -> Option<u64> {
//         unsafe { Some(ffi::nextDF(self.ptr)) }
//     }
// }

// impl Drop for HsList {
//     fn drop(&mut self) {
//         unsafe { hs_free_stable_ptr(self.ptr) }
//     }
// }

fn main() {
    unsafe {
        ffi::potatoInit();
        ffi::hello();
    }

    let boxs: Box<str> = Box::from("helooo".to_string());
    let bs = bytes::HsByteString::new(Box::from(boxs));
    bs.print();
    eprintln!("{:?}", bs.as_slice());
    drop(bs);
    unsafe { ffi::hs_perform_gc() };
    std::thread::sleep(std::time::Duration::from_secs(1));
    unsafe { ffi::hs_perform_gc() };
    std::thread::sleep(std::time::Duration::from_secs(1));

    // let l1 = HsList::new();
    // let mut l2 = HsList::new();
    // l2.next();
    // let v: Vec<(u64, u64)> = l1.zip(l2).take(10).collect();
    // println!("{:?}", v);

    // const N: usize = 1_000_000;
    // let mut l = HsList::new();
    // let before = std::time::Instant::now();
    // println!("{:?}", l.nth(N));
    // let taken = before.elapsed();
    // println!(
    //     "l.nth({}) took {:#?} = {:#?} per step",
    //     N,
    //     taken,
    //     taken / N as u32
    // );

    // let mut list = list::HsList::counter();
    // println!(
    //     "taking 2: [{}, {}]",
    //     list.next().unwrap(),
    //     list.next().unwrap()
    // );
    // let mut cloned = list.clone();
    // println!(
    //     "taking 2: [{}, {}]",
    //     list.next().unwrap(),
    //     list.next().unwrap()
    // );
    // println!(
    //     "taking 2 (cloned): [{}, {}]",
    //     cloned.next().unwrap(),
    //     cloned.next().unwrap()
    // );

    // if false {
    //     let file = std::ffi::CString::new("../dyn/Plug.o").unwrap();
    //     let symbol = std::ffi::CString::new("myCoolList").unwrap();

    //     let mut dyn_ptr = std::ptr::null_mut();
    //     // unsafe {
    //     //     let res = ffi::loadList(
    //     //         file.as_ptr() as _,
    //     //         symbol.as_ptr() as _,
    //     //         &mut dyn_ptr as *mut *mut c_void as _,
    //     //     );
    //     //     println!("res = {:?}", res);
    //     // };
    //     let mut dyn_list = HsList { ptr: dyn_ptr };
    //     println!(
    //         "dyn_list.next() = [{}, {}, {}]",
    //         dyn_list.next().unwrap(),
    //         dyn_list.next().unwrap(),
    //         dyn_list.next().unwrap()
    //     );

    //     let mut dyn_ptr = std::ptr::null_mut();
    //     // unsafe {
    //     //     let res = ffi::loadList(
    //     //         file.as_ptr() as _,
    //     //         symbol.as_ptr() as _,
    //     //         &mut dyn_ptr as *mut *mut c_void as _,
    //     //     );
    //     //     println!("res = {:?}", res);
    //     // };
    //     let mut dyn_list = HsList { ptr: dyn_ptr };
    //     println!(
    //         "dyn_list.next() = [{}, {}, {}]",
    //         dyn_list.next().unwrap(),
    //         dyn_list.next().unwrap(),
    //         dyn_list.nth(1_000_000).unwrap()
    //     );
    // }

    let vec: Vec<u64> = vec![11, 22, 33, 44, 55, 66];
    let hs = list::HsList::from_iter(vec.into_iter());
    // let hs2 = hs.clone();
    // println!("cloned it");
    let vec: Vec<u64> = hs.take(8).collect();
    println!("{:?}", vec);
    // let vec2: Vec<u64> = hs2.collect();
    // println!("{:?}", vec2);

    // loadList
    //   :: CString -- ^ object file
    //   -> CString -- ^ symbol name
    //   -> Ptr (StablePtr (IORef [Word64]))
    //   -- ^ pointer that list will be written to
    //   -> IO Bool

    // // callbacks

    // let mut i = 2;
    // unsafe {
    //     let mut f = move || {
    //         i += 1;
    //         i
    //     };
    //     let (closure, f) = unpack_closure(&mut f);
    //     tester_rs(f, closure);
    // }

    // let mut i = 5;
    // unsafe {
    //     let mut f = move || {
    //         i += 1;
    //         i
    //     };
    //     let (closure, f) = unpack_closure(&mut f);
    //     let stptr = event_rs(f, closure);
    //     let lst = HsList { ptr: stptr };
    //     let v: Vec<u64> = lst.take(10).collect();
    //     println!("{:?}", v);
    // }

    // // dataflow

    // let mut sptr: ffi::HsStablePtr = std::ptr::null_mut();
    // let sptr_ptr = &sptr as *const ffi::HsStablePtr as *mut ffi::HsStablePtr;
    // // let mut i = 0;
    // unsafe {
    //     let mut f = move || {
    //         println!("hello!");
    //         if sptr_ptr.is_null() {
    //             return false
    //         }
    //         println!("sptr_ptr = {:p}", sptr_ptr);
    //         let sptr: ffi::HsStablePtr = *sptr_ptr;
    //         println!("sptr = {:p}", sptr);
    //         if sptr.is_null() {
    //             return false
    //         }
    //         i += 1;
    //         // if i < 3 {
    //         //     return false
    //         // }
    //         let b = ffi::peekB(sptr);
    //         b == 1 && (i % 5 > 2)
    //         // true
    //     };
    //     let (closure, f) = unpack_closure_b(&mut f);
    //     println!("about to get the pointer");
    //     let stptr = df_rs(f, closure);
    //     println!("got the pointer");
    //     sptr = stptr;
    //     println!("set the pointer");
    //     let lst = HsListDF { ptr: stptr };
    //     let v: Vec<u64> = lst.take(20).collect();
    //     println!("{:?}", v);
    // }
}

// type Progress = extern "C" fn(*mut c_void);
//
// /// Unpack a Rust closure, extracting a `void*` pointer to the data and a
// /// trampoline function which can be used to invoke it.
// ///
// /// # Safety
// ///
// /// It is the user's responsibility to ensure the closure outlives the returned
// /// `void*` pointer.
// ///
// /// Calling the trampoline function with anything except the `void*` pointer
// /// will result in *Undefined Behaviour*.
// ///
// /// The closure should guarantee that it never panics, seeing as panicking
// /// across the FFI barrier is *Undefined Behaviour*. You may find
// /// `std::panic::catch_unwind()` useful.
// unsafe fn unpack_closure<F>(closure: &mut F) -> (*mut c_void, extern "C" fn(*mut c_void) -> u64)
// where
//     F: FnMut() -> u64,
// {
//     extern "C" fn trampoline<F>(data: *mut c_void) -> u64
//     where
//         F: FnMut() -> u64,
//     {
//         let tramp_closure: &mut F = unsafe { &mut *(data as *mut F) };
//         (*tramp_closure)()
//     }
//
//     (closure as *mut F as *mut c_void, trampoline::<F>)
// }
//
// unsafe fn unpack_closure_b<F>(closure: &mut F) -> (*mut c_void, extern "C" fn(*mut c_void) -> bool)
// where
//     F: FnMut() -> bool,
// {
//     extern "C" fn trampoline<F>(data: *mut c_void) -> bool
//     where
//         F: FnMut() -> bool,
//     {
//         let tramp_closure: &mut F = unsafe { &mut *(data as *mut F) };
//         (*tramp_closure)()
//     }
//
//     (closure as *mut F as *mut c_void, trampoline::<F>)
// }
//
// unsafe extern fn df_rs(fptr: extern fn(*mut c_void) -> bool, context: *mut c_void) -> ffi::HsStablePtr {
//     let fptr_hs = &fptr as *const extern fn(*mut c_void) -> bool as *const extern fn();
//     ffi::onOutput(Some(*fptr_hs), context)
// }
// unsafe extern fn event_rs(fptr: extern fn(*mut c_void) -> u64, context: *mut c_void) -> ffi::HsStablePtr {
//     let fptr_hs = &fptr as *const extern fn(*mut c_void) -> u64 as *const extern fn();
//     ffi::theEvent(Some(*fptr_hs), context)
// }
// unsafe extern "C" fn tester_rs(fptr: extern fn(*mut c_void) -> u64, context: *mut c_void) {
//     let fptr_hs = &fptr as *const extern fn(*mut c_void) -> u64 as *const extern fn();
//     ffi::tester(Some(*fptr_hs), context)
// }
