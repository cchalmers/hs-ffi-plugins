use crate::ffi;
use std::ffi::c_void;
use std::marker::PhantomData;
use std::mem::MaybeUninit;
use thiserror::Error;

/// A haskell lazy linked list.
pub struct HsList<T> {
    pub ptr: ffi::HsStablePtr,
    phantom: PhantomData<T>,
}

impl<T> Drop for HsList<T> {
    fn drop(&mut self) {
        unsafe { ffi::freeStable(self.ptr) }
    }
}

impl<T> Clone for HsList<T> {
    fn clone(&self) -> Self {
        let ptr = unsafe { ffi::cloneStableRef(self.ptr) };
        HsList {
            ptr,
            phantom: PhantomData,
        }
    }
}

type CbBox<T> = Box<dyn FnMut(*mut T) -> bool>;

extern "C" fn run_list<T>(fptr: *mut c_void, ptr: *mut T) -> bool {
    assert!(!fptr.is_null());
    assert!(!ptr.is_null());
    let fptr = fptr as *mut CbBox<T>;
    unsafe { (*fptr)(ptr) }
}

extern "C" fn free_list<T>(fptr: *mut c_void) {
    assert!(!fptr.is_null());
    let fptr = fptr as *mut CbBox<T>;
    unsafe {
        drop(Box::from_raw(fptr));
    }
}

impl HsList<u64> {
    /// A haskell generated list that counts up from 1.
    pub fn counter() -> HsList<u64> {
        HsList {
            ptr: unsafe { ffi::mkNewList() },
            phantom: PhantomData,
        }
    }
}

/// An error from running a haskell evaluation.
#[derive(Error, Debug)]
pub enum HsException {
    #[error("{0}")]
    Msg(String),
}

pub trait HsFfi {
    unsafe fn try_next_list(hsptr: ffi::HsStablePtr, t: *mut Self) -> Result<bool, HsException>;
    unsafe fn next_list(hsptr: ffi::HsStablePtr, t: *mut Self) -> bool;
    unsafe fn mk_list(
        run_ptr: extern "C" fn(*mut c_void, *mut Self) -> bool,
        free_ptr: extern "C" fn(*mut c_void),
        ctx: *mut c_void,
    ) -> ffi::HsStablePtr;
}

macro_rules! hs_ffi {
    ($ty: ty, $try_next: ident, $next: ident, $mk_list_nm: ident) => {
        impl HsFfi for $ty {
            unsafe fn try_next_list(
                hsptr: ffi::HsStablePtr,
                t: *mut Self,
            ) -> Result<bool, HsException> {
                let mut exception: ffi::HsStablePtr = std::ptr::null_mut();
                let empty =
                    ffi::$try_next(hsptr, t as _, &mut exception as *mut ffi::HsStablePtr as _)
                        != 0;
                if exception.is_null() {
                    Ok(empty)
                } else {
                    let err_msg = ffi::show_exception(exception);
                    let err_msg_iter = ffi::mk_list_iter(err_msg);
                    let list: HsList<char> = HsList::from_ptr(err_msg_iter);
                    Err(HsException::Msg(list.collect()))
                }
            }
            unsafe fn next_list(hsptr: ffi::HsStablePtr, t: *mut Self) -> bool {
                ffi::$next(hsptr, t as _) != 0
            }
            unsafe fn mk_list(
                run_ptr: extern "C" fn(*mut c_void, *mut Self) -> bool,
                free_ptr: extern "C" fn(*mut c_void),
                ctx: *mut c_void,
            ) -> ffi::HsStablePtr {
                use std::mem::transmute;
                // Haskell exports all function pointers as FunPtr(IO ()) so we need to transmute here.
                // This is super unsafe and can easily go very wrong but I don't know a better way.
                let run_ptr = transmute::<
                    extern "C" fn(*mut c_void, *mut $ty) -> bool,
                    extern "C" fn(),
                >(run_ptr);
                let free_ptr = transmute::<extern "C" fn(*mut c_void), extern "C" fn()>(free_ptr);
                ffi::$mk_list_nm(Some(run_ptr), Some(free_ptr), ctx as _)
            }
        }
    };
}

hs_ffi!(i64, tryNextList64, nextList64, foreignListRefI64);
hs_ffi!(i32, tryNextList32, nextList32, foreignListRefI64);
hs_ffi!(i16, tryNextList16, nextList16, foreignListRefI64);
hs_ffi!(i8, tryNextList8, nextList8, foreignListRefI64);
hs_ffi!(u64, tryNextList64, nextList64, foreignListRefU64);
hs_ffi!(u32, tryNextList32, nextList32, foreignListRefU64);
hs_ffi!(u16, tryNextList16, nextList16, foreignListRefU64);
hs_ffi!(u8, tryNextList8, nextList8, foreignListRefU64);
hs_ffi!(char, tryNextListChar, nextListChar, foreignListRefChar);
// hs_ffi!(bool, nextListBool);

impl<T: HsFfi> Iterator for HsList<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        let mut t = MaybeUninit::uninit();
        if unsafe { T::next_list(self.ptr, t.as_mut_ptr()) } {
            Some(unsafe { t.assume_init() })
        } else {
            None
        }
    }
}

impl<T: HsFfi> HsList<T> {
    pub unsafe fn from_ptr(ptr: ffi::HsStablePtr) -> HsList<T> {
        HsList {
            ptr,
            phantom: PhantomData,
        }
    }

    pub fn try_next(&self) -> Option<Result<T, HsException>> {
        let mut t = MaybeUninit::uninit();
        match unsafe { T::try_next_list(self.ptr, t.as_mut_ptr()) } {
            Ok(empty) => {
                if empty {
                    Some(Ok(unsafe { t.assume_init() }))
                } else {
                    None
                }
            }
            Err(msg) => Some(Err(msg)),
        }
    }

    pub fn from_iter<I>(iter: I) -> HsList<T>
    where
        I: IntoIterator<Item = T> + 'static,
    {
        let mut iter = iter.into_iter();
        let closure: Box<dyn FnMut(*mut T) -> bool> =
            Box::new(move |ptr: *mut T| match iter.next() {
                None => false,
                Some(x) => {
                    unsafe { *ptr = x }
                    true
                }
            });
        // need a second box because Box<dyn> is special (could probably avoid it by specialising)
        let boxed_ctx = Box::new(closure); // 📦📦
        let ctx = Box::into_raw(boxed_ctx);

        let ptr = unsafe { T::mk_list(run_list::<T>, free_list::<T>, ctx as _) };
        HsList {
            ptr,
            phantom: PhantomData,
        }
    }
}

// https://stackoverflow.com/a/50413062
#[ouroboros::self_referencing]
pub struct IntoChars {
    string: String,
    #[borrows(string)]
    chars: std::str::Chars<'this>,
}

impl Iterator for IntoChars {
    type Item = char;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.with_mut(|me| me.chars.next())
    }
}

impl From<String> for HsList<char> {
    fn from(string: String) -> HsList<char> {
        Self::from_iter(
            IntoCharsBuilder {
                string,
                chars_builder: |s| s.chars(),
            }
            .build(),
        )
    }
}
