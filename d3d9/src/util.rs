use sq_common::*;
use clap::{Subcommand, Command, FromArgMatches};
use anyhow::Result;
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
        level: Option<u32>,
    },

    /// Stub command for no-operation, does nothing
    #[default]
    Nop,

    /// Exit process
    Exit,
}

/// CLI Frontend for SQ debugger
pub struct DebuggerFrontend {
    last_cmd: Commands
}

/// Private methods
impl DebuggerFrontend {
    fn print_backtrace(bt: dbg::SqBacktrace) {
        println!("Backtrace:");
        for (lvl, info) in bt.into_iter().enumerate() {
            println!("{:03}: {info}", lvl + 1);
        }
    }

    fn print_locals(locals: Vec<SqLocalVar>, lvl: u32) {
        println!("Level {lvl} locals:");
        for SqLocalVar { name, val } in locals {
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
}

/// Public methods
impl DebuggerFrontend {
    pub fn new() -> Self {
        Self { last_cmd: Commands::default() }
    }

    /// Send last parsed args to debugger
    pub fn do_actions(&self, dbg: &mut dbg::SqDebugger) {
        match &self.last_cmd {
            Commands::Step => match dbg.step() {
                Ok(e) => println!("{e}"),
                Err(e) => println!("step failed: {e}"),
            } ,
            Commands::Continue => dbg.resume(),
            Commands::Backtrace => match dbg.get_backtrace() {
                Ok(bt) => Self::print_backtrace(bt),
                Err(e) => println!("failed to get backtrace: {e}"),
            },
            Commands::Locals { level } => 
            if let Some(lvl) = level {
                match dbg.get_locals(*lvl) {
                    Ok(locals) => Self::print_locals(locals, *lvl),
                    Err(e) => println!("failed to get locals: {e}"),
                }
            } else {
                let mut lvl = 1;
                while let Ok(locals) = dbg.get_locals(lvl) {
                    Self::print_locals(locals, lvl);
                    println!();
                    lvl += 1;
                } 
            }
            Commands::Examine { level, name } => {
                let mut var = None;
                if let Some(lvl) = level {
                    match dbg.get_locals(*lvl) {
                        Ok(locals) => {
                            var = locals.into_iter().find(|v| &v.name == name);
                        },
                        Err(e) => {
                            println!("failed to get locals: {e}");
                            return;
                        },
                    }
                } else {
                    let mut lvl = 1;
                    while let Ok(locals) = dbg.get_locals(lvl) {
                        var = locals.into_iter().find(|v| &v.name == name);
                        if var.is_some() { break }
                        lvl += 1;
                    }
                }

                match var {
                    Some(SqLocalVar { name, val }) => 
                        println!("{name}: {typ:?} = {val:#?}", typ = val.get_type()),
                    None => println!("local {name} not found"),
                }
            }

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

/// Bind a function and it's associated Squirrel closure to the object
///
/// ```cpp
/// // Source
/// inline void BindFunc([this], const SQChar* name, void* method, size_t methodSize, SQFUNCTION func, bool staticVar = false) {
///     sq_pushobject(vm, GetObject());
///     sq_pushstring(vm, name, -1);
///
///     SQUserPointer methodPtr = sq_newuserdata(vm, static_cast<SQUnsignedInteger>(methodSize));
///     memcpy(methodPtr, method, methodSize);
///
///     sq_newclosure(vm, func, 1);
///     sq_newslot(vm, -3, staticVar);
///     sq_pop(vm,1); // pop table
/// }
/// ```
pub type BindSQFnFn = unsafe extern "thiscall" fn(
    table: *mut u8,
    name: *const u8,
    method: *mut u8,
    method_size: usize, // usually 4
    sq_fn: sq_common::SQFn,        // sq wrapper func
    static_var: bool    // for static member
);

/// Binds generated SQ module to table
///
/// Sqrat function wrapping chain:
/// BindFunc(.., method) <- SqGlobalFunc<R>(method) <- template with needed argcount <- ~~static~~ fn(hsqvm) -> SQInteger
#[macro_export]
macro_rules! sq_bind_method {
    ($bind_fn:expr, $tab_ptr:expr, $sq_struct:ty) => {
        {
            $bind_fn(
                $tab_ptr as _,
                concat!(stringify!($sq_struct), "\0").as_ptr(),
                <$sq_struct>::rust_fn as _,
                4,
                <$sq_struct>::sq_fn as _,
                true
            );
        }
    };
}
