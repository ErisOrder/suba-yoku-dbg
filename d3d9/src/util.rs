use sq_common::{*, dbg::{SqLocalVarWithLvl}};
use std::{
    sync::atomic,
    fs::File,
};
use clap::{Subcommand, Command, FromArgMatches};
use anyhow::Result;
use serde::{Serialize, Deserialize};
use crate::hooks;


const DEFAULT_STATE_FILENAME: &str = "state.json";

#[derive(clap::ValueEnum, Copy, Clone, Debug)]
enum BoolVal {
    True,
    False,
}

impl From<BoolVal> for bool {
    fn from(value: BoolVal) -> Self {
        match value {
            BoolVal::True => true,
            BoolVal::False => false,
        }
    }
}

#[derive(Subcommand, Debug)]
enum SetCommands {
    /// Activate or deactivate printf hook of sqvm
    PrintfHook {
        #[arg(value_enum)]
        active: BoolVal,
    }
}

#[derive(Subcommand, Debug, Clone, Copy)]
enum BufferCommands {
    /// Create new empty buffer
    #[clap(visible_alias = "n")]
    New,

    /// Delete buffer by number
    #[clap(visible_alias = "d", visible_alias = "del")]
    Delete {
        /// Number of buffer
        num: u32
    },

    /// Edit existing buffer by number
    #[clap(visible_alias = "e")]
    Edit {
        /// Number of buffer
        num: u32
    },

    /// Print buffer by number 
    #[clap(visible_alias = "p")]
    Print {
        /// Number of buffer
        num: u32
    },

    /// List available buffers
    #[clap(visible_alias = "ls")]
    List,
}

/// CLI Frontend commands
#[derive(Subcommand, Debug, Default)]
enum Commands {
    /// Step one debug callback call
    #[clap(visible_alias = "s")]
    Step,

    /// Continue execution
    #[clap(visible_alias = "c")]
    Continue,

    /// Print call backtrace
    #[clap(visible_alias = "bt")]
    Backtrace,

    /// Print local variables list at specified call stack level
    #[clap(visible_alias = "loc")]
    Locals {
        /// Level of call stack. Can be found using backtrace.
        /// If not specified, print all
        level: Option<u32>
    },

    /// Print value of local variable
    #[clap(visible_alias = "x")]
    Examine {
        /// Name of local variable
        name: String,

        /// Specify level of call stack.
        ///
        /// If not specified, print first found local
        level: Option<SqUnsignedInteger>,
    },

    /// Add new breakpoint
    #[clap(visible_alias = "b", visible_alias = "break")]
    BreakpointAdd {
        /// Breakpoint specification.
        ///
        /// Must be in format [file:<src>]:[function]:[line].
        ///
        /// At least 1 condition must be specified.
        spec: String
    },

    /// Enable breakpoint. If number not specified, enable all
    #[clap(visible_alias = "be")]
    BreakpointEnable {
        /// Breakpoint number
        num: Option<u32>
    },

    /// Disable breakpoint. If number not specified, disable all
    #[clap(visible_alias = "bd")]
    BreakpointDisable {
        /// Breakpoint number
        num: Option<u32>
    },

    /// Clear breakpoint. If number not specified, clear all
    #[clap(visible_alias = "bc")]
    BreakpointClear {
        /// Breakpoint number
        num: Option<u32>
    },

    /// List all breakpoints
    #[clap(visible_alias = "bl")]
    BreakpointList,

    /// Compile and run arbitrary squirrel code
    #[clap(visible_alias = "eval")]
    Evaluate {
        /// If specified, enable debugging of compiled script
        #[clap(visible_alias = "dbg", long)]
        debug: bool, 

        /// Choose script buffer to evaluate. If not specified, new buffer will be created.
        buffer: Option<u32>,
    },

    /// Add, remove, edit and view script buffers
    #[clap(visible_alias = "buf")]
    #[command(subcommand)]
    Buffer(BufferCommands),

    /// Continue execution, but print every debug event.
    ///
    /// Warning: due to heavy use of stdout, it may be hard to send stop command to debugger, use breakpoints instead 
    #[clap(visible_alias = "t")]
    Trace,

    /// Set values of different debugging variables
    #[command(subcommand)]
    Set(SetCommands),

    /// Stub command for no-operation, does nothing
    #[default]
    Nop,

    /// Save breakpoints and buffers.
    Save {
        /// File to save state.
        /// If not specified, default file will be used
        file: Option<String>,
    },

    /// Load breakpoints and buffers.
    Load {
        /// File to load state from.
        /// If not specified, default file will be used
        file: Option<String>,
    },

    /// Exit process
    Exit,
}

#[derive(Clone, Serialize, Deserialize)]
 struct SavedState {
    buffers: ScriptBuffers,
    breakpoints: dbg::BreakpointStore,
}

/// CLI Frontend for SQ debugger
pub struct DebuggerFrontend {
    last_cmd: Commands,
    buffers: ScriptBuffers,
    during_eval: bool,
}

/// Private methods
impl DebuggerFrontend {
    fn print_backtrace(bt: dbg::SqBacktrace) {
        println!("Backtrace:");
        for (lvl, info) in bt.into_iter().enumerate() {
            println!("{:03}: {info}", lvl + 1);
        }
    }

    /// Print locals in form
    /// ```rs
    /// Level X locals:
    /// loc: type [= val]
    /// 
    /// Level Y locals:
    /// ...
    /// ```
    fn print_locals(locals: Vec<SqLocalVarWithLvl>) {
        let mut curr_lvl = 0; // Non-existent
        for SqLocalVarWithLvl { var: SqLocalVar { name, val }, lvl } in locals {
            if lvl != curr_lvl {
                println!("\nLevel {lvl} locals:");
                curr_lvl = lvl;
            }

            print!("{name}: {:?}", val.get_type());

            match val {
                DynSqVar::Integer(i) => println!(" = {i}"),
                DynSqVar::Float(f) => println!(" = {f}"),
                DynSqVar::Bool(b) => println!(" = {b}"),
                DynSqVar::String(s) => println!(" = \"{s}\""),
                _ => println!(),
            }
        }
    }

    /// Get CLI parser
    fn cli() -> Command {
        // strip out usage
        const PARSER_TEMPLATE: &str = "\
            {all-args}
        ";

        Commands::augment_subcommands(
            Command::new("repl")
                .multicall(true)
                .arg_required_else_help(false)
                .subcommand_required(true)
                .subcommand_value_name("Command")
                .subcommand_help_heading("Commands")
                .help_template(PARSER_TEMPLATE)
        )
    }

    /// Set debugger variable
    fn set_var(var: &SetCommands) {
        match var {
            SetCommands::PrintfHook { active }
                => hooks::PRINTF_HOOK_ACTIVE.store((*active).into(), atomic::Ordering::Relaxed),
        }
    }

    /// Pretty-print local variable
    fn examine(dbg: &dbg::SqDebugger, name: &str, level: Option<SqUnsignedInteger>) {
        match dbg.get_locals(level) {
            Ok(locs) => match locs.into_iter().find(|v| v.var.name == name) {
                Some(SqLocalVarWithLvl { var: SqLocalVar { name, val }, ..}) =>
                    println!("{name}: {typ:?} = {val}", typ = val.get_type()),
                None => println!("local {name} not found"),
            },
            Err(e) => {
                println!("failed to get locals: {e}");
            },
        }
    }

    /// Parse breakpoint specification 
    fn add_breakpoint(dbg: &dbg::SqDebugger, spec: &str) {
        let mut bp_proto = dbg::SqBreakpoint::new();
        let spec = spec.split(':').collect::<Vec<_>>();

        // Parse src file if present
        let spec = {
            let spec = &spec[..];
            if let ["file", src, ..] = spec[..] {
                bp_proto = bp_proto.src_file(src.to_string());
                &spec[2..]
            } else { spec }
        };

        // Parse remaining
        let bp = match spec {
            [func, line]
            if func.starts_with(|c: char| !c.is_numeric())
            && line.chars().all(|c| c.is_ascii_digit())
                => bp_proto.fn_name(func.to_string())
                    .line(line.parse().unwrap()),

            [line] if line.chars().all(|c| c.is_ascii_digit())
                => bp_proto.line(line.parse().unwrap()),

            [func] if func.starts_with(|c: char| !c.is_numeric())
                => bp_proto.fn_name(func.to_string()),

            [] => bp_proto,

            _ => {
                println!("Invalid breakpoint format");
                return;
            }
        };

        dbg.breakpoints().add(bp);
    }

    /// Create or edit buffer
    fn edit_buffer(prev: Option<&str>) -> Result<String> {
        match scrawl::editor::new()
            .editor("nvim")
            .extension(".nut")
            .contents(prev.unwrap_or_default())
            .open() 
        {
            Ok(s) => Ok(s),
            // Try to open default editor
            Err(_) => Ok(scrawl::with(prev.unwrap_or_default())?),
        }
    }

    /// Execute arbitrary script 
    fn eval_script(&mut self, dbg: &dbg::SqDebugger, debug: bool, buffer: Option<u32>) {
        if self.during_eval {
            println!("failed to evaluate: cannot evaluate during evaluation");
            return;
        }

        let script = match buffer {
            Some(num ) if self.buffers.get(num).is_some() => {
                self.buffers.get(num).unwrap()
            }, 
            _ => match Self::edit_buffer(None) {
                Ok(s) => {
                    let num = self.buffers.add(s);
                    self.buffers.get(num).unwrap()
                },
                Err(e) => {
                    println!("failed to open editor: {e}");
                    return;
                },
            },
        };

        self.during_eval = true;

        match dbg.execute(script.clone(), debug) {
            Ok(res) => println!("evaluation result: {res}"),
            Err(e) => println!("failed to evaluate: {e}"),
        }

        self.during_eval = false;
    }

    /// Process buffer commands
    fn manipulate_buffer(&mut self, cmd: BufferCommands) {
        match cmd {
            BufferCommands::New => match Self::edit_buffer(None) {
                Ok(s) =>println!("new buffer number: {}", self.buffers.add(s)),
                Err(e) => println!("failed to open editor: {e}"),
            }

            BufferCommands::Delete { num } => self.buffers.delete(num),
            BufferCommands::Edit { num } => 
            if let Some(b) = self.buffers.get(num) {
                match Self::edit_buffer(Some(b)) {
                    Ok(s) => self.buffers.replace(num, s),
                    Err(e) => println!("failed to open editor: {e}"),
                }
            } else {
                println!("no such buffer")
            }
        
            BufferCommands::Print { num } => 
            if let Some(b) = self.buffers.get(num) {
                println!("{b}");
            } else {
                println!("no such buffer")
            }
            
            BufferCommands::List => println!("{}", self.buffers),
        }
    }

    /// Save state to file
    fn save(state: SavedState, path: &str) -> Result<()> {
        let f = File::create(path)?;
        serde_json::to_writer_pretty(&f, &state)?;
        Ok(())
    }

    /// Load state from file
    fn load(path: &str) -> Result<SavedState> {
        let f = File::open(path)?;
        let state: SavedState = serde_json::from_reader(f)?;
        Ok(state)
    }
}

/// Public methods
impl DebuggerFrontend {
    pub fn new() -> Self {
        Self { 
            last_cmd: Commands::default(),
            buffers: ScriptBuffers::new(),
            during_eval: false
        }
    }

    /// Send last parsed args to debugger
    pub fn do_actions(&mut self, dbg: &mut dbg::SqDebugger) {
        match &self.last_cmd {
            Commands::Step => if let Err(e) = dbg.step() {
                println!("step failed: {e}");
            }

            Commands::Continue => dbg.resume(),

            Commands::Backtrace => match dbg.get_backtrace() {
                Ok(bt) => Self::print_backtrace(bt),
                Err(e) => println!("failed to get backtrace: {e}"),
            }

            Commands::Locals { level } =>
            match dbg.get_locals(*level) {
                Ok(locals) => Self::print_locals(locals),
                Err(e) => println!("failed to get locals: {e}"),
            }

            Commands::Examine { level, name } => Self::examine(dbg, name, *level),
            Commands::BreakpointAdd { spec } => Self::add_breakpoint(dbg, spec),
            Commands::BreakpointEnable { num } => dbg.breakpoints().enable(*num, true),
            Commands::BreakpointDisable { num } => dbg.breakpoints().enable(*num, false),
            Commands::BreakpointClear { num } => dbg.breakpoints().remove(*num),
            Commands::BreakpointList => println!("Breakpoints:\n{}", dbg.breakpoints()),

            Commands::Evaluate { debug , buffer } 
                => self.eval_script(dbg, *debug, *buffer),

            Commands::Buffer(cmd) => self.manipulate_buffer(*cmd),

            Commands::Trace => if let Err(e) = dbg.start_tracing() {
                println!("trace failed: {e}")
            }

            Commands::Load { file } => 
            match Self::load(file.as_deref().unwrap_or(DEFAULT_STATE_FILENAME)) {
                Ok(SavedState { buffers, breakpoints }) => {
                    self.buffers = buffers;
                    dbg.set_breakpoints(breakpoints);
                },
                Err(e) => println!("Failed to load state: {e}"),
            },

            Commands::Save { file } => {
                let state = SavedState { 
                    buffers: self.buffers.clone(),
                    breakpoints: dbg.breakpoints().clone()
                };

                if let Err(e) = Self::save(state, file.as_deref().unwrap_or(DEFAULT_STATE_FILENAME)) {
                    println!("Failed to save state: {e}")
                }
            }


            Commands::Set(var) => Self::set_var(var),
            Commands::Nop => (),
            Commands::Exit => std::process::exit(0),
        }
    }

    /// Save arguments to internal buffer, if successful
    pub fn parse_args(&mut self, args: &str) -> Result<()> {
        match Self::cli().try_get_matches_from(args.trim().split(' ')) {
            Ok(m) => {
                self.last_cmd = Commands::from_arg_matches(&m)?;
                Ok(())
            },
            Err(e) => Err(e.into()),
        }
    }
}

/// Struct that holds multiple string with script
#[derive(Clone, Serialize, Deserialize)]
struct ScriptBuffers {
    store: Vec<(u32, String)>,
    counter: u32,
}

impl ScriptBuffers {
    /// Create new ScriptBuffers
    pub fn new() -> Self {
        Self { store: vec![], counter: 1 }
    }

    /// Add buffer. Returns added buffer number
    pub fn add(&mut self, buf: String) -> u32 {
        self.store.push((self.counter, buf));
        self.counter += 1;
        self.counter - 1
    }

    /// Get buffer by number
    pub fn get(&mut self, number: u32) -> Option<&String> {
        self.store.iter().find(|(n, _)| *n == number).map(|(_, b)| b)
    }

    /// Replace buffer by number. If buffer with specified number doesn't exist, do nothing
    pub fn replace(&mut self, number: u32, buf: String) {
        if let Some(b) = self.store.iter_mut().find(|(n, _)| *n == number) {
            *b = (number, buf);
        }
    }

    /// Delete buffer by number
    pub fn delete(&mut self, number: u32) {
        self.store.retain(|(n, _)| *n != number)
    }
}

impl std::fmt::Display for ScriptBuffers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const NUM_FIELD: usize = 8;

        if self.store.is_empty() {
            return write!(f, "no buffers available");
        }

        write!(f, "{:<NUM_FIELD$}content", "number")?;
        for (n, buf) in &self.store {
            // print separating newline
            writeln!(f)?;
            let line = buf.lines().next();
            write!(f, "{n:<NUM_FIELD$}{} ...", if let Some(l) = line{ l } else { "" })?;
        }
        Ok(())
    }
}