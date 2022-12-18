use std::{time::Duration, sync::{Arc, Mutex, MutexGuard}};
use anyhow::{Result, bail};
use atomic::{Atomic, Ordering};
use crossbeam::channel::{bounded, unbounded, Receiver, Sender};
use serde::{Serialize, Deserialize};
use crate::sq::*;


const RECV_TIMEOUT: Duration = Duration::from_secs(10);

const BP_NUMBER_FIELD: usize = 8;
const BP_ENABLED_FIELD: usize = 10;

#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum ExecState {
    Running,
    Halted
}

pub enum DebugMsg {
    Step,
    Backtrace,
    Trace,
    /// Level, Depth
    Locals(Option<SqUnsignedInteger>, SqUnsignedInteger),
    Eval(String, bool),
}

/// SqLocalVar annotated with level
#[derive(Clone, PartialEq, PartialOrd, Eq, Debug, Hash)]
pub struct SqLocalVarWithLvl {
    pub var: SqLocalVar,
    pub lvl: SqUnsignedInteger,
}

impl std::fmt::Display for DebugEventWithSrc {
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

#[derive(Clone, PartialEq, PartialOrd, Eq, Debug, Hash)]
pub enum DebugResp {
    Backtrace(SqBacktrace),
    Locals(Option<Vec<SqLocalVarWithLvl>>),
    EvalResult(String),
}

/// Struct for holding breakpoint data. At least 1 condition field must be specified for it to work 
#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash, Serialize, Deserialize)]
pub struct SqBreakpoint {
    pub line: Option<SqInteger>,
    pub fn_name: Option<String>,
    pub src_file: Option<String>,
    pub enabled: bool,
    pub number: u32,
}

impl SqBreakpoint {
    /// Create new blank breakpoint
    pub fn new() -> Self {
        Self {
            line: None,
            fn_name: None,
            src_file: None,
            enabled: true,
            number: 0,
        }
    }

    /// Set line condition
    pub fn line(mut self, line: SqInteger) -> Self {
        self.line = Some(line);
        self
    }

    /// Set function name condition
    pub fn fn_name(mut self, fn_name: String) -> Self {
        self.fn_name = Some(fn_name);
        self
    }

    /// Set source file condition
    pub fn src_file(mut self, src_file: String) -> Self {
        self.src_file = Some(src_file);
        self
    }

    /// Try to match debug event with breakpoint conditions
    pub fn match_event(&self, event: &DebugEventWithSrc) -> bool {
        let DebugEventWithSrc { event, src } = event; 

        // Check source file, if specified and do not match, return false
        match (&self.src_file, src) {
            (Some(_), None) => return false,
            (Some(self_src), Some(src)) if self_src != src
              => return false,
            _ => (),
        }

        // Same as upper chunk
        let line_match = |l_opt| match (self.line, l_opt) {
            (Some(_), None) => false,
            (Some(line), Some(l)) if line != l => false,
            _ => true,
        };

        // Also same
        let name_match = |n: &str| match &self.fn_name {
            Some(name) if name == n => true,
            Some(name) if name != n => false,
            _ => true
        };
        
        match event {
            DebugEvent::Line(line) 
                => self.fn_name.is_none() && line_match(Some(*line)),

            DebugEvent::FnCall(name, line)
            | DebugEvent::FnRet(name, line) 
                => name_match(name) && line_match(*line)
        }
    }
    
    /// Enable or disable breakpoint
    pub fn enable(&mut self, en: bool) {
        self.enabled = en;
    }
}

impl std::fmt::Display for SqBreakpoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let SqBreakpoint { line, fn_name, src_file, enabled, number } = self;
        write!(f, "{number:<BP_NUMBER_FIELD$}{enabled:<BP_ENABLED_FIELD$}")?;

        if src_file.is_some() {
            write!(f, "file:")?;
        }

        // TODO: Replace with some of itertools crate method
        let line = line.as_ref().map(|line| line.to_string());
        let strvec: Vec<_> = [src_file, fn_name, &line].into_iter().flatten().cloned().collect();
        let joined = strvec.join(":");

        f.write_str(&joined)?;
         
        Ok(())
    }
}

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash, Serialize, Deserialize)]
pub struct BreakpointStore { 
    store: Vec<SqBreakpoint>,
    counter: u32,
}

impl BreakpointStore {
    /// Create new empty store
    pub fn new() -> Self {
        Self { store: vec![], counter: 1 }
    }

    /// Add new breakpoint
    pub fn add(&mut self, mut bp: SqBreakpoint) {
        bp.number = self.counter;
        self.counter += 1;
        self.store.push(bp);
    }

    /// Remove breakpoint by number.
    /// If number not specified, remove all
    pub fn remove(&mut self, num: Option<u32>) {
        self.store.retain(|bp| matches!(num, Some(num) if bp.number != num));
    }

    /// Match event with every enabled breakpoint in store
    pub fn match_event(&self, event: &DebugEventWithSrc) -> Option<&SqBreakpoint> {
        self.store.iter().find(|bp| bp.enabled && bp.match_event(event))
    }

    /// Enable or disable breakpoint by number.
    /// If number not specified, enable/disable all
    pub fn enable(&mut self, num: Option<u32>, en: bool) {
        self.store.iter_mut().filter(
            |bp| !matches!(num, Some(num) if bp.number != num)
        ).for_each(|bp| bp.enable(en));
    }
}

impl std::fmt::Display for BreakpointStore {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.store.is_empty() {
            return write!(f, "no breakpoints registered")
        }
        writeln!(f, "{:<BP_NUMBER_FIELD$}{:<BP_ENABLED_FIELD$}location", "number", "enabled")?;   
        for bp in &self.store {
            writeln!(f, "{bp}")?;
        }
        Ok(())
    }
}

type DebugEventBundle = (DebugEventWithSrc, Option<SqBreakpoint>);

/// SQ Debugger middleware (backend is debug hook closure)
pub struct SqDebugger{
    /// State of execution, shared with hook
    exec_state: Arc<Atomic<ExecState>>,

    /// Channel for sending commands to hook
    sender: Sender<DebugMsg>,

    /// Channel for receiveing responses from hook
    receiver: Receiver<DebugResp>,

    /// Channel only for receiving events
    event_receiver: Receiver<DebugEventBundle>,

    /// Breakpont store, shared with hook
    breakpoints: Arc<Mutex<BreakpointStore>>,

    /// VM being debugged
    vm: SafeVm,
}

impl SqDebugger
{
    /// Attach debugger to SQVM through setting debug hook.
    pub fn attach(vm: SafeVm) -> SqDebugger {

        let (tx, rx) = unbounded();
        let (resp_tx, resp_rx) = bounded(0);
        let (event_tx, event_rx) = bounded(0);

        let mut dbg = Self {
            exec_state: Arc::new(Atomic::new(ExecState::Halted)),
            sender: tx,
            receiver: resp_rx,
            event_receiver: event_rx,
            breakpoints: Arc::new(Mutex::new(BreakpointStore::new())),
            vm,
        };

        let exec_state = dbg.exec_state.clone();
        let breakpoints = dbg.breakpoints.clone();
        let mut debugging = true;
        let mut tracing = false;

        // Attached debugger will receive messages and respond to them
        dbg.vm.set_debug_hook(move |e, vm| {
            
            // if debugging disabled during hook call
            if !debugging {
                return;
            }

            let bp = breakpoints.lock().unwrap()
                .match_event(&e)
                .cloned();


            // If VM was running and ran into breakpoint, halt it 
            let state = if bp.is_some() {
                tracing = false;
                exec_state.store(ExecState::Halted, Ordering::Relaxed);
                ExecState::Halted
            } else {
                exec_state.load(Ordering::Relaxed)
            };
    
            // If tracing active, or vm ran into brakpoint, or 
            // step cmd was received on previous debug hook call,
            // send debug event back with optional breakpoint
            if tracing || state == ExecState::Halted {
                event_tx.send((e, bp)).unwrap();

                // Stop tracing, if vm was halted 
                if state == ExecState::Halted {
                    tracing = false;
                }
            }

            loop {
                if let Ok(msg) = rx.try_recv() { match msg {
                    // Expected immediate receive on other end for all sending cmds

                    DebugMsg::Step => break,
                    DebugMsg::Backtrace => {
                        let mut bt = vec![];

                        let stack_size = vm.call_stack_len() as u32;

                        for lvl in 1..stack_size {
                            if let Ok(info) = vm.get_stack_info(lvl) {
                                bt.push(info);
                            }
                        }

                        resp_tx.send(DebugResp::Backtrace(bt)).unwrap();
                    },

                    DebugMsg::Trace => {
                        tracing = true;
                        exec_state.store(ExecState::Running, Ordering::Relaxed);
                        break;
                    }

                    DebugMsg::Locals(lvl_opt, depth) => 'locals: {
                        let stack_size = vm.call_stack_len() as u32;

                        // Store all locals if level isn`t specified  
                        let lvl = if let Some(lvl) = lvl_opt { lvl } else { 1 };

                        if lvl >= stack_size || lvl < 1 {
                            resp_tx.send(DebugResp::Locals(None)).unwrap();
                            break 'locals;
                        }

                        let mut v = vec![];

                        for lvl in lvl..stack_size {
                            let mut idx = 0;
                            while let Ok(Some(loc)) = vm.get_local(lvl, idx, Some(depth)) {
                                v.push(SqLocalVarWithLvl { var: loc, lvl });
                                idx += 1;
                            }

                            if lvl_opt.is_some() {
                                break;
                            }
                        }

                        resp_tx.send(DebugResp::Locals(if v.is_empty() { None } else { Some(v) })).unwrap();
                    },
                    DebugMsg::Eval(script, debug) => {

                        // it`s a bit of black magic, so compiler can`t infer that...
                        // hook will be called again during compiled closure call,
                        // so this closure implicitly becomes recursive
                        #[allow(unused_assignments)] {
                            debugging = debug;
                        }

                        let res = vm.execute_script(script, "eval.nut".into());

                        let res = match res {
                            Ok(_) => "Ok".into(),
                            Err(e) => e.to_string(),
                        };
                        
                        // TODO: Improve reporting with debug enabled
                        if !debug {
                            resp_tx.send(DebugResp::EvalResult(res)).unwrap();
                        }

                        debugging = true;
                    }
                }}

                if exec_state.load(Ordering::Relaxed) == ExecState::Running {
                    break;
                }
                std::thread::sleep(Duration::from_millis(50));
            }
        });

        dbg
    }

    /// Continue execution, but send every debug event
    pub fn start_tracing(&self) -> Result<()> {
        self.sender.send(DebugMsg::Trace)?;
        Ok(())
    }

    /// Resume execution
    pub fn resume(&self) {
        self.exec_state.store(ExecState::Running, Ordering::Relaxed);
    }

    /// Get event receiver
    pub fn event_rx(&self) -> &Receiver<DebugEventBundle> {
        &self.event_receiver
    }

    /// Get breakpoint store
    pub fn breakpoints(&self) -> MutexGuard<BreakpointStore> {
       self.breakpoints.lock().unwrap()
    }

    /// Set breakpoint store
    pub fn set_breakpoints(&self, points: BreakpointStore) {
        *self.breakpoints.lock().unwrap() = points; 
    }

    /// Halt execution by blocking vm on debug hook call
    pub fn halt(&self) {
        self.exec_state.store(ExecState::Halted, Ordering::Relaxed);
    }

    /// Unlock current debug hook call
    pub fn step(&self) -> Result<()> {
        self.sender.send(DebugMsg::Step)?;
        Ok(())
    }

    /// Get local variables and their values at specified level.
    /// 
    /// May be pretty expensive
    /// 
    /// Args:
    /// - `lvl` - if `None`, return locals gathered from all levels.
    /// 
    /// - `depth` - Depth of eager containers (table, array, etc.) expansion.
    ///   - 0 - do not expand.
    ///   - 1 - expand this container.
    ///   - 2 - expand this container and all children
    ///   - 3.. - and so on
    pub fn get_locals(&self, lvl: Option<SqUnsignedInteger>, depth: SqUnsignedInteger) -> Result<Vec<SqLocalVarWithLvl>> {
        self.sender.send(DebugMsg::Locals(lvl, depth))?;
        match self.receiver.recv_timeout(RECV_TIMEOUT) {
            Ok(DebugResp::Locals(loc)) => 
                if let Some(loc) = loc {
                    Ok(loc)
                } else { match lvl {
                    Some(lvl) => bail!("no locals at level {lvl}"),
                    None => bail!("no locals at all levels"),
                }},
            Ok(r) => bail!("{r:?}: expected locals"),
            Err(e) => bail!(e)
        }
    }
    
    /// Compile and execute arbitrary squirrel script
    /// 
    /// Args:
    /// - `debug` - if `true`, enable debugging of compiled script
    pub fn execute(&self, script: String, debug: bool) -> Result<String> {
        self.sender.send(DebugMsg::Eval(script, debug))?;

        if !debug {
            match self.receiver.recv_timeout(RECV_TIMEOUT) {
                Ok(DebugResp::EvalResult(res)) => Ok(res),
                Ok(r) => bail!("{r:?}: expected eval result"),
                Err(e) => bail!(e)
            }
        } else {
            Ok("ok".into())
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
        match self.receiver.recv_timeout(RECV_TIMEOUT) {
            Ok(DebugResp::Backtrace(bt)) => Ok(bt),
            Ok(r) => bail!("{r:?}: expected backtrace"),
            Err(e) => bail!(e)
        }
    }

    pub fn exec_state(&self) -> ExecState {
        self.exec_state.load(Ordering::Relaxed)
    }
}
