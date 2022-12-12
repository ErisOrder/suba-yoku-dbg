use sq_common::{*, dbg::SqLocalVarWithLvl};
use std::sync::atomic;
use clap::{Subcommand, Command, FromArgMatches};
use anyhow::Result;
use crate::hooks;

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


    /// Set values of different debugging variables
    #[command(subcommand)]
    Set(SetCommands),

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

    fn set_var(var: &SetCommands) {
        match var {
            SetCommands::PrintfHook { active }
                => hooks::PRINTF_HOOK_ACTIVE.store((*active).into(), atomic::Ordering::Relaxed),
        }
    }

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
            match dbg.get_locals(*level) {
                Ok(locals) => Self::print_locals(locals),
                Err(e) => println!("failed to get locals: {e}"),
            }
            Commands::Examine { level, name } => Self::examine(dbg, name, *level),
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
