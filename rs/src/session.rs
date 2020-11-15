use crate::dynamic::Dynamic;
use crate::dynamic::Typeable;
use crate::ffi;
use crate::list::HsList;
use std::ffi::CString;
use std::path::Path;
use thiserror::Error;

pub struct Session {
    ptr: ffi::HsStablePtr,
}

/// An error from running a haskell evaluation.
#[derive(Error, Debug)]
pub enum EvalError {
    #[error("interrupted")]
    Interrupt,
    #[error("exit_code({0})")]
    ExitCode(i32),
    #[error("{0}")]
    Msg(String),
}

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

    pub fn run_expr_dyn(&self, expr: &str) -> Result<Dynamic, EvalError> {
        let cstr = std::ffi::CString::new(expr).expect("module_name");
        let mut dyn_ptr = std::ptr::null_mut();
        let res = unsafe {
            ffi::run_expr_dyn(
                self.ptr,
                cstr.as_ptr() as *mut _,
                &mut dyn_ptr as *mut _ as *mut _,
            )
        };
        let dynamic = Dynamic { ptr: dyn_ptr };
        match res {
            0 => Ok(dynamic),
            1 => Err(EvalError::Interrupt),
            2 => Err(EvalError::ExitCode(
                isize::from_dynamic(&dynamic).expect("error_code") as i32,
            )),
            3 => {
                let chars = HsList::<char>::from_dynamic(&dynamic).expect("error_code");
                Err(EvalError::Msg(chars.collect()))
            }
            _ => panic!("bad eval error exit code {}", res),
        }
    }

    pub fn run_expr<T: Typeable + Sized>(&self, expr: &str) -> Result<T, EvalError> {
        let cstr = std::ffi::CString::new(expr).expect("run_expr");
        let mut ptr = std::ptr::null_mut();
        let typerep = T::typerep();
        let res = unsafe {
            ffi::run_expr_with_type(
                self.ptr,
                typerep.ptr,
                cstr.as_ptr() as *mut _,
                &mut ptr as *mut _ as *mut _,
            )
        };
        match res {
            0 => Ok(unsafe { T::deref_stable(ptr) }),
            1 => Err(EvalError::Interrupt),
            2 => {
                let dynamic = Dynamic { ptr };
                Err(EvalError::ExitCode(isize::from_dynamic(&dynamic).expect("error_code") as i32,))
            }
            3 => {
                let dynamic = Dynamic { ptr };
                let chars = HsList::<char>::from_dynamic(&dynamic).expect("error_code");
                Err(EvalError::Msg(chars.collect()))
            }
            _ => panic!("bad eval error exit code {}", res),
        }
    }

    pub fn run_decl(&self, decl: &str) -> Result<(), EvalError> {
        let cstr = std::ffi::CString::new(decl).expect("decl");
        let mut ptr = std::ptr::null_mut();
        let res = unsafe {
            ffi::run_decl(
                self.ptr,
                cstr.as_ptr() as *mut _,
                &mut ptr as *mut _ as *mut _,
            )
        };
        match res {
            0 => Ok(()),
            1 => Err(EvalError::Interrupt),
            2 => {
                let dynamic = Dynamic { ptr };
                Err(EvalError::ExitCode(isize::from_dynamic(&dynamic).expect("error_code") as i32,))
            }
            3 => {
                let dynamic = Dynamic { ptr };
                let chars = HsList::<char>::from_dynamic(&dynamic).expect("error_code");
                Err(EvalError::Msg(chars.collect()))
            }
            _ => panic!("bad eval error exit code {}", res),
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
