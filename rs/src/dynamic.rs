// use crate::ffi;
// use std::ffi::c_void;
// use std::ffi::CString;

// loadList
//   :: CString -- ^ object file
//   -> CString -- ^ symbol name
//   -> Ptr (StablePtr (IORef [Word64]))
//   -- ^ pointer that list will be written to
//   -> IO Bool

// pub unsafe fn load_symbol<P: AsRef<std::path::Path>>(path: P, symbol: &str) -> ffi::HsStablePtr {
//     let path = CString::new(path.as_ref().to_str().expect("not utf8 path")).unwrap();
//     let symbol = std::ffi::CString::new(symbol).unwrap();

//     let mut dyn_ptr = std::ptr::null_mut();
//     let res = ffi::loadList(
//         path.as_ptr() as _,
//         symbol.as_ptr() as _,
//         &mut dyn_ptr as *mut *mut c_void as _,
//     );
//     if res == 0 {
//         panic!("failed load_symbol");
//     }
//     dyn_ptr
// }
