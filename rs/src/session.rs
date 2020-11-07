use crate::dynamic::Dynamic;
use crate::ffi;

pub struct Session {
    ptr: ffi::HsStablePtr,
}

impl Session {
    pub fn new(lib: Option<&str>) -> Session {
        let lib_cstr = lib.map(|str| { eprintln!("using lib {}", str); std::ffi::CString::new(str).expect("lib name")});
        let lib_ptr = if let Some(cstr) = &lib_cstr {
            eprintln!("using cstr {:?}", cstr);
            cstr.as_ptr() as *mut _
        } else {
            std::ptr::null_mut()
        };
        eprintln!("lib cstr has addr {:p}", lib_ptr);
        Session {
            ptr: unsafe { ffi::new_session(lib_ptr) },
        }
    }

    pub fn import_modules(&self, module_names: &[&str]) {
        let cstrs: Vec<_> = module_names
            .iter()
            .map(|&nm| std::ffi::CString::new(nm).expect("module_name"))
            .collect();
        let cstr_ptrs: Vec<_> = cstrs.iter().map(|cstr| cstr.as_ptr()).collect();
        unsafe { ffi::import_modules(self.ptr, cstrs.len() as i64, cstr_ptrs.as_ptr() as *mut _) }
    }

    pub fn run_expr(&self, expr: &str) {
        let cstr = std::ffi::CString::new(expr).expect("module_name");
        unsafe { ffi::run_expr(self.ptr, cstr.as_ptr() as *mut _) }
    }

    pub fn run_expr_dyn(&self, expr: &str) -> Option<Dynamic> {
        let cstr = std::ffi::CString::new(expr).expect("module_name");
        let mut dyn_ptr = std::ptr::null_mut();
        let success = unsafe {
            ffi::run_expr_dyn(
                self.ptr,
                cstr.as_ptr() as *mut _,
                &mut dyn_ptr as *mut _ as *mut _,
            )
        };
        if success == 1 {
            Some(Dynamic { ptr: dyn_ptr })
        } else {
            None
        }
    }

    pub fn debugging(&self) {
        unsafe {
            ffi::debugging(self.ptr)
        }
    }
}

impl Drop for Session {
    fn drop(&mut self) {
        unsafe { ffi::cleanup_session(self.ptr) }
    }
}
