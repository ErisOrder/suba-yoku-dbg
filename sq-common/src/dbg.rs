use std::{time::Duration, sync::{Arc, Mutex}};

use crate::sq::*;
//use squirrel2_kaleido_rs::*;

use log::debug;

#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum ExecState {
    Running,
    Halted
}


pub struct SqDebugger<'a>{
    exec_state: Arc<Mutex<ExecState>>,
    vm: SafeVm<'a>,
}


impl<'a> SqDebugger<'a>
{
    pub fn attach(vm: SafeVm<'a>) -> SqDebugger<'a> {

        let mut dbg = Self {
            exec_state: Arc::new(Mutex::new(ExecState::Halted)),
            vm,
        };

        let exec_state = dbg.exec_state.clone();

        dbg.vm.set_debug_hook(Box::new(move |e, src| {
            debug!("{src:?}: {e:?}");
            
            if *exec_state.lock().unwrap() == ExecState::Halted {
                debug!("Waiting to continue...");
            }
            loop {
                if *exec_state.lock().unwrap() == ExecState::Running {
                    break;
                }
                std::thread::sleep(Duration::from_millis(50));
            }
        }));

        dbg
    }

    pub fn resume(&mut self) {
        *self.exec_state.lock().unwrap() = ExecState::Running;
    }

    pub fn halt(&mut self) {
        *self.exec_state.lock().unwrap() = ExecState::Halted;
    }

    pub fn exec_state(&self) -> ExecState {
        *self.exec_state.lock().unwrap()
    }
}
