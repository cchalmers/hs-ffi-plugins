use crate::ffi;
use std::ffi::c_void;
use std::marker::PhantomData;
use std::mem::MaybeUninit;

/// A haskell lazy linked list.
pub struct HsList<T> {
    ptr: ffi::HsStablePtr,
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

extern "C" fn run_u64_list(fptr: *mut c_void, ptr: *mut u64) -> bool {
    assert!(!fptr.is_null());
    assert!(!ptr.is_null());
    let fptr = fptr as *mut CbBox<u64>;
    unsafe { (*fptr)(ptr) }
}

extern "C" fn free_u64_list(fptr: *mut c_void) {
    assert!(!fptr.is_null());
    let fptr = fptr as *mut CbBox<u64>;
    unsafe {
        drop(Box::from_raw(fptr));
    }
}

// pub fn weird() -> *mut std::ffi::c_void {
//     let closure: Box<dyn FnMut(*mut u64) -> bool> = Box::new(|ptr: *mut u64| true);
//     let ptr = Box::into_raw(closure);
//     ptr as _
// }

// fn weirder(ptr: *mut std::ffi::c_void) -> bool {
//     let ptr = ptr as *mut dyn FnMut(*mut u64) -> bool;
//     let bx = unsafe {Box::from_raw(ptr)};
//     let mut a = 0;
//     bx(&mut a)
// }

impl HsList<u64> {
    pub unsafe fn from_ptr(ptr: ffi::HsStablePtr) -> HsList<u64> {
        HsList {
            ptr,
            phantom: PhantomData,
        }
    }

    pub fn from_iter<I>(mut iter: I) -> HsList<u64>
    where
        I: Iterator<Item = u64>,
    {
        let closure: Box<dyn FnMut(*mut u64) -> bool> =
            Box::new(move |ptr: *mut u64| match iter.next() {
                None => false,
                Some(x) => {
                    unsafe { *ptr = x }
                    true
                }
            });
        // need a second box because Box<dyn> is special (could probably avoid it by specialising)
        let boxed_ctx = Box::new(closure); // 📦📦
        let ctx = Box::into_raw(boxed_ctx);

        // all these different ways to not get the right pointer
        // println!("run_u64_list {:p}", run_u64_list);
        // println!("run_ptr {:p}", run_ptr);
        // let run_ptr_ptr = &mut run_u64_list
        //     as &mut _ // extern fn(*mut dyn FnMut(*mut u64) -> bool, *mut u64) -> bool;
        //     as *mut _ // extern fn(*mut dyn FnMut(*mut u64) -> bool, *mut u64) -> bool;
        //     as *mut extern fn();
        // println!("&run_u64_list {:p}", unsafe {&run_u64_list});
        // println!("run_ptr_ptr {:p}", unsafe {run_ptr_ptr});
        // println!("*run_ptr_ptr {:p}", unsafe {*run_ptr_ptr});

        // Haskell doesn't export the right types for function pointers, all functions look like
        // FunPtr(IO ()). Transmute is the only way I've found to do this.
        let run_ptr = unsafe {
            std::mem::transmute::<extern "C" fn(*mut c_void, *mut u64) -> bool, extern "C" fn()>(
                run_u64_list,
            )
        };
        let free_ptr = unsafe {
            std::mem::transmute::<extern "C" fn(*mut c_void), extern "C" fn()>(free_u64_list)
        };
        let ptr = unsafe { ffi::foreignListRef(Some(run_ptr), Some(free_ptr), ctx as _) };
        HsList {
            ptr,
            phantom: PhantomData,
        }
    }

    /// A haskell generated list that counts up from 1.
    pub fn counter() -> HsList<u64> {
        HsList {
            ptr: unsafe { ffi::mkNewList() },
            phantom: PhantomData,
        }
    }
}

pub trait HsFfi {
    unsafe fn next_list(hsptr: ffi::HsStablePtr, t: *mut Self) -> bool;
}

macro_rules! hs_ffi {
    ($ty: ty, $nm: ident) => {
        impl HsFfi for $ty {
            unsafe fn next_list(hsptr: ffi::HsStablePtr, t: *mut Self) -> bool {
                ffi::$nm(hsptr, t as _) != 0
            }
        }
    }
}

hs_ffi!(u64, nextList64);
hs_ffi!(u32, nextList32);
hs_ffi!(u16, nextList16);
hs_ffi!(u8, nextList8);
hs_ffi!(bool, nextListBool);

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
