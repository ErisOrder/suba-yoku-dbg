use super::api::{self, VmRawApi};


/// Vm safety
pub mod safety {
    use super::api::VmRawApi;
        
    /// Vm drop specialization helper trait
    pub trait VmDrop where Self: Sized {
        fn drop_vm(vm: &mut super::Vm<Self>);
    }

    /// Automatically dropped
    pub struct Safe;

    /// Manually dropped
    pub struct Unsafe;

    /// Cannot be dropped (drops on main vm drop)
    pub struct Friend;

    impl VmDrop for Safe {
        fn drop_vm(vm: &mut super::Vm<Self>) {
            unsafe { vm.api().close() }
        }
    }

    impl VmDrop for Unsafe {
        fn drop_vm(_: &mut super::Vm<Self>) {}
    }
    
    impl VmDrop for Friend {
        fn drop_vm(_: &mut super::Vm<Self>) {}
    }
}

/// Struct for accessing raw api methods of the vm
pub struct VmApi(api::HSQUIRRELVM);

impl api::VmRawApi for VmApi {
    fn handle(&self) -> squirrel2_kaleido_rs::HSQUIRRELVM {
        self.0
    }
}

impl<S> Drop for Vm<S> where S: safety::VmDrop {
    fn drop(&mut self) {
        S::drop_vm(self)
    }
} 

pub struct Vm<S> where S: safety::VmDrop  {
    api: VmApi,
    safety: S
}

impl<S> Vm<S> where S: safety::VmDrop {
    /// Get underlying api
    pub fn api(&self) -> &VmApi {
        &self.api
    }    
}

impl Vm<safety::Unsafe> {
    /// Close this vm and all friend vms
    pub fn close(self) {
        unsafe { self.api().close(); }
    }
}

