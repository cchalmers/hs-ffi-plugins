// use crate::dynamic::Dynamic;
// use crate::dynamic::Typeable;
use crate::ffi;
// use crate::list::HsList;
use std::ffi::CString;
use std::path::Path;
// use thiserror::Error;

pub struct ModIface {
    ptr: ffi::HsStablePtr,
}

impl ModIface {
    pub fn read<P: AsRef<Path>>(path: P) -> ModIface {
        let path_cstr =
            CString::new(path.as_ref().to_str().expect("libpath not utf8"))
                .expect("modiface path name");
        let path_ptr = path_cstr.as_ptr() as *mut _;
        ModIface {
            ptr: unsafe { ffi::read_bin_iface(path_ptr) },
        }
    }

    pub fn pp(&self) {
        unsafe {
            ffi::pp_iface(self.ptr);
        }
    }
}

impl Drop for ModIface {
    fn drop(&mut self) {
        unsafe { ffi::freeStable(self.ptr) }
    }
}
