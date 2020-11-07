use crate::ffi;

pub struct Dynamic {
    pub ptr: ffi::HsStablePtr,
}

pub struct TypeRep {
    pub ptr: ffi::HsStablePtr,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Fingerprint(pub u128);

impl Dynamic {
    pub fn fingerprint(&self) -> Fingerprint {
        self.typerep().fingerprint()
    }

    pub fn typerep(&self) -> TypeRep {
        TypeRep {
            ptr: unsafe { ffi::dyn_type_rep(self.ptr) },
        }
    }

    unsafe fn dynamic_value(&self) -> ffi::HsStablePtr {
        ffi::dyn_val(self.ptr)
    }
}

impl TypeRep {
    pub fn fingerprint(&self) -> Fingerprint {
        let mut a: u64 = 0;
        let mut b: u64 = 0;
        unsafe {
            ffi::type_rep_fingerprint(
                self.ptr,
                &mut a as *mut _ as *mut _,
                &mut b as *mut _ as *mut _,
            );
        }
        Fingerprint((a as u128) << 64 | (b as u128))
    }

    pub fn list_of(&self) -> TypeRep {
        TypeRep {
            ptr: unsafe { ffi::list_type_rep(self.ptr) },
        }
    }
}

pub trait Typeable {
    fn typerep() -> TypeRep;
    fn from_dynamic(dynamic: &Dynamic) -> Option<Self>
    where
        Self: Sized;
}

impl Typeable for u64 {
    fn typerep() -> TypeRep {
        TypeRep {
            ptr: unsafe { ffi::word64_type_rep() },
        }
    }

    fn from_dynamic(dynamic: &Dynamic) -> Option<u64> {
        if dynamic.fingerprint() == Self::typerep().fingerprint() {
            unsafe { Some(ffi::deref_word64(dynamic.dynamic_value())) }
        } else {
            None
        }
    }
}

// foreign export ccall int_type_rep :: IO (StablePtr SomeTypeRep)
// foreign export ccall word_type_rep :: IO (StablePtr SomeTypeRep)
// foreign export ccall list_type_rep :: StablePtr SomeTypeRep -> IO (StablePtr SomeTypeRep)
// foreign export ccall type_rep_fingerprint :: StablePtr SomeTypeRep -> Ptr Word64 -> Ptr Word64 -> IO ()
// foreign export ccall dyn_type_rep :: StablePtr Dynamic -> IO (StablePtr SomeTypeRep)
// foreign export ccall dyn_val :: StablePtr Dynamic -> IO (StablePtr Any)
