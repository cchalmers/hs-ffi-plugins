use crate::ffi;

pub struct Session {
    ptr: ffi::HsStablePtr
}

impl Session {
    pub fn new() -> Session {
        Session {
            ptr: unsafe { ffi::new_session() },
        }
    }

    pub fn import_module(&self, module_name: &str) {
        let cstr = std::ffi::CString::new(module_name).expect("module_name");
        unsafe {
            ffi::import_module(self.ptr, cstr.as_ptr() as *mut _)
        }
    }

    pub fn run_expr(&self, expr: &str) {
        let cstr = std::ffi::CString::new(expr).expect("module_name");
        unsafe {
            ffi::run_expr(self.ptr, cstr.as_ptr() as *mut _)
        }
    }
}

impl Drop for Session {
    fn drop(&mut self) {
        unsafe {
            ffi::cleanup_session(self.ptr)
        }
    }
}
