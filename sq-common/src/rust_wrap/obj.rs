
use super::types::*;
use super::api::SQObject;
use super::vm::{Vm, safety::VmDrop};
use super::get::SqGetResult;
use super::push::SqPush;

/// Strong reference to squirrel vm object with RAII
pub struct SqObjectRef<'vm, S> where S: VmDrop {
    obj: SQObject,
    vm: &'vm Vm<S>
}

impl<'vm, S> Drop for SqObjectRef<'vm, S> where S: VmDrop {
    fn drop(&mut self) {
        self.vm.dec_ref(&mut self.obj);
    }
}

impl<'vm, S> SqObjectRef<'vm, S> where S: VmDrop {
    /// Get type of referenced object
    pub fn get_type(&self) -> SqType {
        self.obj._type.into()
    }

    /// Get a reference to object on the stack
    pub fn get(vm: &'vm Vm<S>, idx: isize) -> SqGetResult<Self> {
        let mut obj = vm.get_stack_obj(idx)
            .map_err(|e| e.into_stack_error("failed to get object handle"))?;
        vm.inc_ref(&mut obj);
        Ok(SqObjectRef {
            obj,
            vm,
        })
    }
}

// TODO: Implementing SqGet for SqObjectRef is possible, but requires
// changing `&self` in SqGet methods to `self` and implementing this trait not for VM,
// but for &VM due to lifetime elision that assumes 
//
// get(&'1 self, ..) -> SqObjectRef<'2, ..>
//       ^                           ^
//
// while what we really need is 
// get(&'1 self, ..) -> SqObjectRef<'1, ..>
//       ^                           ^ 

impl<'vm, S> SqPush<SqObjectRef<'vm, S>> for Vm<S> where S: VmDrop {
    type Output = ();
    
    fn push(&self, val: SqObjectRef<'vm, S>) {
        self.push_stack_obj(&val.obj);
    }
}