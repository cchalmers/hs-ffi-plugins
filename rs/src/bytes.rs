use std::ffi::c_void;
use crate::ffi;

pub struct HsByteString {
    ptr: ffi::HsStablePtr,
}

impl Drop for HsByteString {
    fn drop(&mut self) {
        unsafe { ffi::freeStable(self.ptr) }
    }
}

impl HsByteString {
    pub fn new(bytes: Box<[u8]>) -> HsByteString {
        let len = bytes.len();
        let ptr = Box::into_raw(bytes);
        eprintln!("ptr is {:p}", ptr);
        unsafe extern "C" fn drop_box(env: usize, ptr: *mut u8) {
            let slice = std::slice::from_raw_parts_mut(ptr, env);
            std::ptr::drop_in_place(slice as *mut [u8])
        }
        let drop_box = unsafe {std::mem::transmute::<
            unsafe extern "C" fn(env: usize, ptr: *mut u8),
            unsafe extern "C" fn(),
        >(drop_box)};
        unsafe {
            HsByteString { ptr: ffi::newByteString(Some(drop_box as _), len as _, ptr as _, len as i64) }
        }
    }

    pub fn as_slice(&self) -> &[u8] {
        self
    }

    pub fn print(&self) {
        unsafe {
            ffi::printBS(self.ptr)
        }
    }
}

impl std::ops::Deref for HsByteString {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        unsafe {
            let mut len: i64 = 0;
            let mut ptr: *const u8 = std::ptr::null();
            ffi::byteStringParts(self.ptr, &mut ptr as *mut *const u8 as *mut c_void, &mut len as *mut i64 as *mut c_void);
            eprintln!("called byteStringParts");
            eprintln!("I think len = {} and ptr = {:p}", len, ptr);
            std::slice::from_raw_parts(ptr, len as usize)
        }
    }
}


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

