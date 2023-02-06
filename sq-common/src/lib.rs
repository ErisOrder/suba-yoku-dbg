#![allow(non_upper_case_globals)]
#![feature(try_blocks)]
#![feature(concat_idents)]

mod rust_wrap;
pub use rust_wrap::{  
    vm::{self, Vm, safety},
    get::{self, SqGet},
    push::{self, SqPush, IntoPushResult},
    obj::{self, SqObjectRef},
    throw::{self, SqThrow},
    iter,
    api,
    types::*
};

/// Re-export
pub use indexmap::IndexMap;
pub use sq_macro::*;

mod util;
pub mod dbg;
pub mod error;
