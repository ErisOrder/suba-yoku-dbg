use std::{time::Duration, sync::{Arc, Mutex, mpsc}};

use crate::sq::*;
//use squirrel2_kaleido_rs::*;

use log::debug;

#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum ExecState {
    Running,
    Halted
}

pub enum DebugMsg {
    Step,
    DebugMessages(bool)
}

pub struct SqDebugger<'a>{
    exec_state: Arc<Mutex<ExecState>>,
    msg_pipe: mpsc::Sender<DebugMsg>,
    vm: SafeVm<'a>,
}


impl<'a> SqDebugger<'a>
{
    pub fn attach(vm: SafeVm<'a>) -> SqDebugger<'a> {

        let (tx, rx) = mpsc::channel();

        let mut dbg = Self {
            exec_state: Arc::new(Mutex::new(ExecState::Halted)),
            msg_pipe: tx,
            vm,
        };

        let exec_state = dbg.exec_state.clone();
        let mut debug_msg = true;

        dbg.vm.set_debug_hook(Box::new(move |e, src| {
            if debug_msg {
                debug!("{src:?}: {e:?}");
            }
            
            loop {
                if let Ok(msg) = rx.try_recv() { match msg {
                    DebugMsg::Step => break,
                    DebugMsg::DebugMessages(en) => debug_msg = en,
                }}

                if *exec_state.lock().unwrap() == ExecState::Running {
                    break;
                }
                std::thread::sleep(Duration::from_millis(50));
            }
        }));

        dbg
    }

    pub fn resume(&self) {
        *self.exec_state.lock().unwrap() = ExecState::Running;
    }

    pub fn halt(&self) {
        *self.exec_state.lock().unwrap() = ExecState::Halted;
    }

    pub fn step(&self) {
        self.msg_pipe.send(DebugMsg::Step).expect("Failed to send step cmd");
    }

    pub fn enable_debug_messages(&self, en: bool) {
        let msg = DebugMsg::DebugMessages(en);
        self.msg_pipe.send(msg).expect("Failed to send debug messages cmd");
    }

    pub fn exec_state(&self) -> ExecState {
        *self.exec_state.lock().unwrap()
    }
}
