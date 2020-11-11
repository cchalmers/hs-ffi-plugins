use crate::dynamic::Dynamic;
use crate::ffi;
use std::ffi::CString;
use std::path::Path;

pub struct Session {
    ptr: ffi::HsStablePtr,
}

/// An error from running a haskell evaluation.
#[derive(Debug)]
pub enum EvalError {
    Interrupt,
    ExitCode(i32),
    Msg(String),
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SuperError is here!")
    }
}

impl std::error::Error for EvalError {}

impl Session {
    pub fn new<P: AsRef<Path>>(lib: Option<P>) -> Session {
        let lib_cstr = lib.map(|path| {
            CString::new(path.as_ref().to_str().expect("libpath not utf8")).expect("lib name")
        });
        let lib_ptr = if let Some(cstr) = &lib_cstr {
            cstr.as_ptr() as *mut _
        } else {
            std::ptr::null_mut()
        };
        Session {
            ptr: unsafe { ffi::new_session(lib_ptr) },
        }
    }

    pub fn set_verbosity(&self, verbosity: usize) {
        unsafe { ffi::set_verbosity(self.ptr, verbosity as _) }
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

    pub fn run_expr_dyn(&self, expr: &str) -> Result<Dynamic, EvalError> {
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
            Ok(Dynamic { ptr: dyn_ptr })
        } else {
            Err(EvalError::ExitCode(3))
        }
    }

    /// These are the `-i` arguments normally given to the cmdline.
    pub fn set_import_paths(&self, imports: &[&str]) -> bool {
        let cstrs: Vec<_> = imports
            .iter()
            .map(|&nm| std::ffi::CString::new(nm).expect("module_name"))
            .collect();
        let cstr_ptrs: Vec<_> = cstrs.iter().map(|cstr| cstr.as_ptr()).collect();
        unsafe {
            ffi::set_import_paths(self.ptr, cstrs.len() as i64, cstr_ptrs.as_ptr() as *mut _)
        };
        true // fixme
    }

    /// Just like `:load`ing stuff.
    pub fn set_load_paths(&self, module_paths: &[&str]) -> bool {
        let cstrs: Vec<_> = module_paths
            .iter()
            .map(|&nm| std::ffi::CString::new(nm).expect("module_name"))
            .collect();
        let cstr_ptrs: Vec<_> = cstrs.iter().map(|cstr| cstr.as_ptr()).collect();
        unsafe { ffi::load_modules(self.ptr, cstrs.len() as i64, cstr_ptrs.as_ptr() as *mut _) };
        true // fixme
    }

    // foreign export ccall load_modules :: StablePtr Session -> Int -> Ptr CString -> IO Word64
    // foreign export ccall set_import_paths :: StablePtr Session -> Int -> Ptr CString -> IO ()

    pub fn debugging(&self) {
        unsafe { ffi::debugging(self.ptr) }
    }
}

impl Drop for Session {
    fn drop(&mut self) {
        unsafe { ffi::cleanup_session(self.ptr) }
    }
}
