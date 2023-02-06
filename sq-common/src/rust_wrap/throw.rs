
use crate::error::*;
use super::vm::{Vm, safety::VmDrop};

/// Trait for throwing errors to vm
pub trait SqThrow<T> {
    /// Throw as a SQ exception to the vm
    fn throw(&self, throwable: T);
}

impl<S> SqThrow<SqNativeClosureError> for Vm<S> where S: VmDrop {
    fn throw(&self, throwable: SqNativeClosureError) {
        self.throw_error(throwable.to_string())
    }
}

impl<S> SqThrow<SqStackError> for Vm<S> where S: VmDrop {
    fn throw(&self, throwable: SqStackError) {
        self.throw_error(throwable.to_string())
    }
}

impl<S> SqThrow<anyhow::Error> for Vm<S> where S: VmDrop {
    fn throw(&self, throwable: anyhow::Error) {
        let msg = throwable.chain()
            .fold(String::new(), |mut msg, e| {
                msg += "\nerror: ";
                msg += &e.to_string();
                msg
            });
        self.throw_error(msg);
    }
}

