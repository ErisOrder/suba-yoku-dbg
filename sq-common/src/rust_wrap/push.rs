use indexmap::IndexMap;

use crate::error::*;
use super::types::*;
use super::vm::{Vm, safety::VmDrop, SqVoidUserPointer};
use super::get::SqGet;
use super::api::{VmRawApi, HSQUIRRELVM, SqFunction};

pub type SqPushResult = Result<(), SqStackError>;
/// Helper trait that defines shared behaviour 
/// for implementing fallible [SqPush].
pub trait IntoPushResult { 
    /// Convert struct to Result
    fn into_result(self) -> SqPushResult;
    fn default_self() -> Self;
}

impl IntoPushResult for () {
    /// Infallible
    fn into_result(self) -> SqPushResult {
        Ok(())
    }

    fn default_self() -> Self {}
}

impl IntoPushResult for Result<(), SqStackError> {
    /// Just pass through
    fn into_result(self) -> SqPushResult {
        self
    }

    fn default_self() -> Self {
        Ok(())
    }
}

/// Trait for pushing values to vm stack
pub trait SqPush<T> {
    /// Most [SqPush] implementations are infallible,
    /// so we need this associated type for more complex cases
    /// like rust containers
    type Output: IntoPushResult;
    
    /// Push a value to the vm stack
    fn push(&self, val: T) -> Self::Output;
}

impl<S> SqPush<SqUnit> for Vm<S> where S: VmDrop {
    type Output = ();
    
    fn push(&self, _: SqUnit) {}
}

impl<S> SqPush<SqNull> for Vm<S> where S: VmDrop {
    type Output = ();
    
    #[inline]
    fn push(&self, _: SqNull) {
        self.api().push_null();
    }
}

impl<S> SqPush<bool> for Vm<S> where S: VmDrop {
    type Output = ();
    
    #[inline]
    fn push(&self, val: bool) {
        self.api().push_bool(val);
    }
}

impl<S> SqPush<isize> for Vm<S> where S: VmDrop {
    type Output = ();
    
    #[inline]
    fn push(&self, val: isize) {
        self.api().push_integer(val);
    }
}

impl<S, T> SqPush<SqUserPointer<T>> for Vm<S> where S: VmDrop {
    type Output = ();
    
    fn push(&self, val: SqUserPointer<T>) {
        unsafe { self.api().push_userpointer(val as _) };
    }
}

impl<S> SqPush<SqFloat> for Vm<S> where S: VmDrop {
    type Output = ();
    
    #[inline]
    fn push(&self, val: SqFloat) {
        self.api().push_float(val);
    }
}

impl<S> SqPush<&str> for Vm<S> where S: VmDrop {
    type Output = ();
    
    #[inline]
    fn push(&self, val: &str) {
        unsafe { self.api().push_string(val.as_ptr(), val.len() as _) }
    }
}

impl<S> SqPush<String> for Vm<S> where S: VmDrop {
    type Output = ();
    
    #[inline]
    fn push(&self, val: String) {
        self.push(val.as_str());
    }
}

impl<S> SqPush<SqUserData> for Vm<S> where S: VmDrop {
    type Output = ();
    
    #[inline]
    fn push(&self, val: SqUserData) {
        let val = val.unwrap();
        unsafe { 
            let ptr = self.api().new_userdata(val.len() as _);
            std::ptr::copy(val.as_ptr() as _, ptr, val.len());
        }
    }
}

impl<S> SqPush<SqFunction> for Vm<S> where S: VmDrop {
    type Output = ();
    
    #[inline]
    fn push(&self, val: SqFunction) {
        self.new_closure(val, 0)
    }
}

impl<S> SqPush<SqBoxedClosure> for Vm<S> where S: VmDrop {
    type Output = ();
    
    fn push(&self, val: SqBoxedClosure) {
        // Indirection to make fat pointer usable through FFI
        let clos_box = Box::new(val); 

        #[allow(clippy::borrowed_box)]
        extern "C" fn glue(hvm: HSQUIRRELVM) -> isize {
          
            let mut vm = unsafe { Vm::from_handle(hvm).into_friend() };

            let top = vm.api().stack_top();
            let closure_box: SqUserData = vm.get(top)
                .expect("Failed to get closure box ptr");

            let closure: &mut SqBoxedClosure = unsafe {
                let closure_box: usize =  std::ptr::read(closure_box.unwrap().as_ptr() as _) ;
                &mut *(closure_box as *mut _)
            };

            closure(&mut vm)
        }

        extern "C" fn release_hook(ptr: SqVoidUserPointer, _: isize) -> isize {
            // Received ptr is pointer to pointer (of former box) to box
            unsafe { 
                let closure_box: *mut SqBoxedClosure = std::ptr::read(ptr as _) ;
                let _ = Box::from_raw(closure_box);
            };
            1
        }
        
        let raw = Box::into_raw(clos_box);
        let data: Vec<_> = (raw as usize).to_ne_bytes().into();

        self.push(SqUserData::from(data));
        self.set_release_hook(-1, release_hook).expect("Failed to set box release hook");
        self.new_closure(glue, 1);
    }
}

impl<S, T> SqPush<Option<T>> for Vm<S> 
where 
    S: VmDrop,
    Vm<S>: SqPush<T>
{
    /// Fallible for fallible inner types, infallible otherwise
    type Output = <Self as SqPush<T>>::Output;
    
    fn push(&self, val: Option<T>) -> Self::Output {
        match val {
            Some(v) => self.push(v),
            None => {
                SqPush::<SqNull>::push(self, SqNull);
                Self::Output::default_self()     
            },
        }
    }
}

impl<S, T> SqPush<Vec<T>> for Vm<S> 
where 
    S: VmDrop, 
    Vm<S>: SqPush<T> + SqPush<isize> 
{
    type Output = SqPushResult; 
    
    fn push(&self, val: Vec<T>) -> Self::Output {
        self.new_array(val.len());
        for (index, elem) in val.into_iter().enumerate() {
            self.push(index as isize);
            self.push(elem).into_result()?;
            self.slot_set(-3)
                .map_err(|e| e.into_stack_error("failed to set array slot"))?;
        }
        Ok(())
    }
}

impl<S, K, V> SqPush<IndexMap<K, V>> for Vm<S>
where 
    S: VmDrop,
    Vm<S>: SqPush<K> + SqPush<V>,
{
    type Output = SqPushResult;
    
    fn push(&self, val: IndexMap<K, V>) -> Self::Output {
        self.api().new_table();

        for (key, val) in val.into_iter() {
            self.push(key).into_result()?;
            self.push(val).into_result()?;

            self.new_slot(-3, false)
                .map_err(|e| e.into_stack_error("failed to set table slot"))?;
        }
        Ok(())
    }
}

impl<S> SqPush<DynSqVar> for Vm<S> where S: VmDrop {
    type Output = SqPushResult;

    #[allow(clippy::unit_arg)]    
    fn push(&self, val: DynSqVar) -> Self::Output {
        match val {
            DynSqVar::Null => self.push(SqNull).into_result(),
            DynSqVar::Integer(i) => self.push(i).into_result(),
            DynSqVar::String(s) => self.push(s.as_str()).into_result(),
            DynSqVar::Array(v) => self.push(v).into_result(),
            DynSqVar::Float(f) => self.push(f).into_result(),
            DynSqVar::Bool(b) => self.push(b).into_result(),
            DynSqVar::Table(t) => self.push(t).into_result(),
            DynSqVar::UserData(u) => self.push(u).into_result(),
            _ => unimplemented!(),
        }
    }
}

