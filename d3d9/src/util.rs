use sq_common::dbg;
use clap::{Subcommand, Command, FromArgMatches};

/// CLI Frontend commands
#[derive(Subcommand, Debug, Default)]
enum Commands {
    /// Step one debug callback call
    #[default]
    #[clap(visible_alias = "s")]
    Step,

    /// Continue execution
    #[clap(visible_alias = "c")]
    Continue,

    /// Print call stack
    Stack,

    /// Exit process
    Exit,
}

/// CLI Frontend for SQ debugger
pub struct DebuggerFrontend {
    last_cmd: Commands
}

impl DebuggerFrontend {
    pub fn new() -> Self {
        Self { last_cmd: Commands::default() }
    }

    /// Send last parsed args to debugger
    pub fn do_actions(&self, dbg: &mut dbg::SqDebugger) {
        match self.last_cmd {
            Commands::Step => dbg.step(),
            Commands::Continue => dbg.resume(),
            Commands::Stack => dbg.print_call_stack(),
            Commands::Exit => std::process::exit(0),
        }
    }

    /// Save arguments to internal buffer, if successful
    pub fn parse_args(&mut self, args: &str) {
        match Self::cli().try_get_matches_from(args.trim().split(' ')) {
            Ok(m) => {
                self.last_cmd = Commands::from_arg_matches(&m).unwrap()
            },
            Err(e) => match e.kind() {
                clap::error::ErrorKind::MissingSubcommand => println!("command is not specified"),
                clap::error::ErrorKind::DisplayHelpOnMissingArgumentOrSubcommand => (),
                _ => println!("{e}"),
            },
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
