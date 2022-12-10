use std::{time::Duration, sync::{Arc, Mutex, mpsc}};
use anyhow::{Result, bail};
use crate::sq::*;
//use squirrel2_kaleido_rs::*;

#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum ExecState {
    Running,
    Halted
}

pub enum DebugMsg {
    Step,
    Backtrace,
}

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub struct EventWithSrc {
    event: DebugEvent,
    src: Option<String>
}

impl std::fmt::Display for EventWithSrc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let src = if let Some(src_f) = &self.src { src_f } else { "??" };
        match &self.event {
            DebugEvent::Line(line) => write!(f, "line: {src}:{line}"),
            DebugEvent::FnCall(name, line) => 
                write!(f, 
                    "call: {src}:{name} ({ln})",
                    ln = if let Some(line) = line { line.to_string() } else { "??".into() }
                ),
            DebugEvent::FnRet(name, line) =>                 
                write!(f, 
                    "ret:  {src}:{name} ({ln})",
                    ln = if let Some(line) = line { line.to_string() } else { "??".into() }
                ),
        }
    }
}

pub type SqBacktrace = Vec<SqStackInfo>;

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum DebugResp {
    /// event, src
    Event(EventWithSrc),
    Backtrace(SqBacktrace)
}

pub struct SqDebugger<'a>{
    exec_state: Arc<Mutex<ExecState>>,
    sender: mpsc::Sender<DebugMsg>,
    receiver: mpsc::Receiver<DebugResp>,
    vm: SafeVm<'a>,
}


impl<'a> SqDebugger<'a>
{
    /// Attach debugger to SQVM through setting debug hook.
    pub fn attach(vm: SafeVm<'a>) -> SqDebugger<'a> {

        let (tx, rx) = mpsc::channel();
        let (resp_tx, resp_rx) = mpsc::sync_channel(0);

        let mut dbg = Self {
            exec_state: Arc::new(Mutex::new(ExecState::Halted)),
            sender: tx,
            receiver: resp_rx,
            vm,
        };

        let exec_state = dbg.exec_state.clone();

        // Attached debugger will receive messages and respond to them
        dbg.vm.set_debug_hook(Box::new(move |e, src, vm| {

            // Vm was halted or step cmd was received on previous debug hook call
            // So send debug event back
            // This will block until msg isn`t received
            if *exec_state.lock().unwrap() == ExecState::Halted {
                resp_tx.send(DebugResp::Event(EventWithSrc {
                    event: e,
                    src
                })).unwrap();
            }
            
            loop {
                if let Ok(msg) = rx.try_recv() { match msg {
                    // Expected immediate receive on other end for all sending cmds

                    DebugMsg::Step => break,
                    DebugMsg::Backtrace => {
                        let mut bt = vec![];
                        let mut lvl = 1;
                        while let Ok(info) = vm.get_stack_info(lvl) {
                            bt.push(info);
                            lvl += 1;
                        }
                        resp_tx.send(DebugResp::Backtrace(bt)).unwrap();
                    },
                }}

                if *exec_state.lock().unwrap() == ExecState::Running {
                    break;
                }
                std::thread::sleep(Duration::from_millis(50));
            }
        }));

        dbg
    }

    /// Resume execution
    pub fn resume(&self) {
        *self.exec_state.lock().unwrap() = ExecState::Running;
    }

    /// Get internal message receiver
    /// 
    /// May be useful to manage complicated states such as getting message from suspended vm thread 
    pub fn receiver(&self) -> &mpsc::Receiver<DebugResp> {
        &self.receiver
    }

    /// Halt execution by blocking vm on debug hook call
    /// 
    /// Returns last received event if `no_recv` is `false`
    pub fn halt(&self, no_recv: bool) -> Result<Option<EventWithSrc>> {
        let prev = {
            let mut state = self.exec_state.lock().unwrap();
            let prev = *state;
            *state = ExecState::Halted;
            prev
        };
        
        if prev == ExecState::Running && !no_recv {
            match self.receiver.recv_timeout(Duration::from_millis(500)) {
                Ok(DebugResp::Event(e)) => Ok(Some(e)),
                Ok(r) => bail!("{r:?}: expected event"),
                Err(e) => bail!("{e}")
            }
        } else { Ok(None) }
    }

    /// Unlock current debug hook call
    /// 
    /// Returns received event
    pub fn step(&self) -> Result<EventWithSrc> {
        self.sender.send(DebugMsg::Step)?;
        match self.receiver.recv_timeout(Duration::from_millis(500)) {
            Ok(DebugResp::Event(e)) => Ok(e),
            Ok(r) => bail!("{r:?}: expected event"),
            Err(e) => bail!("{e}")
        }
    }

    /// Request backtrace from vm thread, where
    /// ```text
    /// vec_start     vec_end
    /// ^^^^^^^^^     ^^^^
    /// current_fn -> root
    /// ```
    pub fn get_backtrace(&self) -> Result<SqBacktrace> {
        self.sender.send(DebugMsg::Backtrace)?;
        match self.receiver.recv_timeout(Duration::from_millis(500)) {
            Ok(DebugResp::Backtrace(bt)) => Ok(bt),
            Ok(r) => bail!("{r:?}: expected backtrace"),
            Err(e) => bail!("{e}")
        }
    }

    pub fn exec_state(&self) -> ExecState {
        *self.exec_state.lock().unwrap()
    }
}
