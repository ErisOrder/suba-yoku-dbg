use thiserror::Error;
use crate::rust_wrap::{SqType, SqInteger};

pub type SqVmResult<T> = std::result::Result<T, SqVmError>;

/// Evaluates to [SqVmError::InvalidType] if type of object is invalid
#[macro_export]
macro_rules! sq_validate {
    ($check:expr, $($valid_type:path),+) => {
        match $check {
            $(| $valid_type )+ => Ok(()),
            other => Err($crate::error::SqVmError::InvalidType {
                expected: &[ $($valid_type, )+ ],
                received: Some(other),
                msg: Some("expected one of valid types")
            })
        } 
    }
}

/// Error, that can occur during native closure execution
#[derive(Debug, Error)]
pub enum SqNativeClosureError {
    #[error("argument {idx} error: {reason}")]
    ArgError {
        idx: usize,
        reason: SqStackError     
    },
    #[error("return value error: {0}")]
    OutputError(SqStackError),
    #[error("vararg {idx} error: {reason}")]
    VarArgError {
        idx: usize,
        reason: SqStackError     
    }
}

pub type SqCompilerResult<T> = Result<T, SqCompilerError>;

/// Error received from SQ compiler
#[derive(Debug, Error)]
pub enum SqCompilerError {
    #[error("compile error: {src_file}:{line}:{column}: {description}")]
    CompileError {
        line: SqInteger,
        column: SqInteger,
        description: String,
        src_file: String,
    },
    #[error("stack error: {0}")]
    StackError(#[from] SqStackError),
    #[error(transparent)]
    Other(#[from] anyhow::Error)
}

#[derive(Debug, Error)]
pub enum SqStackErrorReason {
    #[error(transparent)]
    SqVmError(SqVmError),
    #[error("reached max expansion depth")]
    MaxDepthReached,
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

#[derive(Debug, Error)]
#[error("{msg}: {reason}")]
pub struct SqStackError {
    reason: SqStackErrorReason,
    msg: &'static str    
}

/// Strong-typed representation of SQVM errors
#[derive(Debug, Error)]
pub enum SqVmError {
    #[error("invalid type: expected {expected:?}, received {} ({})",
         .received.map(|t| format!("{t:?}")).unwrap_or_default(), .msg.unwrap_or(""))]
    InvalidType {
        expected: &'static [SqType],
        received: Option<SqType>,
        msg: Option<&'static str>
    },
    #[error("invalid typemask")]
    InvalidTypemask,
    #[error("the class is locked")]
    LockedClass,
    #[error("invalid typetag")]
    InvalidTypetag,
    #[error("delegate cycle")]
    DelegateCycle,
    #[error("index error: {0}")]
    IndexError(&'static str),
    #[error("call failed")]
    CallFailed,
    #[error("cannot resume idle vm")]
    CannotResumeIdleVm,
    #[error("squirrel io error")]
    SqIoError,
    #[error("cannot pop from empty array")]
    EmptyArray,
    #[error("cannot resize array to negative size")]
    NegativeSize,
    #[error("invalid stream")]
    InvalidStream,
    #[error("{}", .0.as_deref().unwrap_or("unknown error"))]
    Other(Option<String>)
}

impl SqVmError {
    /// Parse original error string, received from VM.
    /// All mistakes in error sentences are to match original ones...  
    pub fn parse<T>(s: T) -> Self where T: AsRef<str> + Into<String> {
        use SqVmError::*;
        use SqType::*;
    
        match s.as_ref() {
            "rawset works only on array/table/class and instance" => InvalidType { 
                expected: &[Array, Table, Class, Instance],
                received: None,
                msg: Some("rawset failed")
            },
            "invalid base type" => InvalidType { 
                expected: &[Class],
                received: None,
                msg: Some("failed to create class")
            },
            "invalid param type" => InvalidType { 
                expected: &[Instance, Class],
                received: None,
                msg: Some("failed to create instance")
            },
            "the type doesn't have a default delegate" => InvalidType {
                expected: &[
                    Table, Array, String, Integer, Generator, 
                    Closure, Thread, Class, Instance, WeakRef,
                ],
                received: None,
                msg: Some("no default delegate")
            },
            "the object is not a closure" => InvalidType {
                expected: &[Closure], 
                received: None,
                msg: None 
            },
            "native closure expected"
            | "the object is not a nativeclosure" => InvalidType { 
                expected: &[NativeClosure],
                received: None,
                msg: None
            },
            "the target is not a closure" => InvalidType { 
                expected: &[Closure, NativeClosure],
                received: None,
                msg: Some("bindenv failed")
            },
            "invalid environment" => InvalidType { 
                expected: &[Table, Class, Instance],
                received: None,
                msg: Some("bindenv failed")
            },
            "clear only works on table and array" => InvalidType { 
                expected: &[Table, Array],
                received: None,
                msg: Some("clear failed")
            },
            "ivalid type" => InvalidType { 
                expected: &[Table, Null],
                received: None,
                msg: Some("failed to set root table")
            },
            "ivalid type, expected table" => InvalidType { 
                expected: &[Table],
                received: None,
                msg: Some("failed to set const table")
            },
            "invalid object type" => InvalidType { 
                expected: &[UserData, Class],
                received: None,
                msg: Some("failed to get or set typetag")
            },
            "the object is not a class instance" => InvalidType { 
                expected: &[Instance],
                received: None,
                msg: None
            },
            "the object is not a class" => InvalidType { 
                expected: &[Class],
                received: None,
                msg: None
            },
            "null key"
            | "null is not a valid key" => InvalidType { 
                expected: &[],
                received: Some(Null),
                msg: None
            },
            "wrong type" => InvalidType { 
                expected: &[Table, UserData],
                received: None,
                msg: Some("failed to get delegate")
            },
            "only generators can be resumed" => InvalidType { 
                expected: &[Generator],
                received: None,
                msg: Some("cannot resume not a generator")
            },
            "the object must be a weakref" => InvalidType { 
                expected: &[WeakRef],
                received: None,
                msg: Some("cannot get weakref value")
            },
            "cannot iterate a generator" => InvalidType { 
                expected: &[Generator],
                received: None,
                msg: Some("cannot iterate a generator")
            },
            "invalid typemask" => InvalidTypemask,
            "the class is locked" => LockedClass,
            "invalid type tag" => InvalidTypetag,
            "delagate cycle" => DelegateCycle,
            "index out of range" => IndexError("index out of range"),
            "the index doesn't exist" => IndexError("index does not exist"),
            "call failed" => CallFailed,
            "cannot resume a vm that is not running any code" => CannotResumeIdleVm,
            "io error" => SqIoError,
            "invalid free var index" => IndexError("invalid free var index"),
            "wrong index" => IndexError("wrong attribute index"),
            "empty array" => EmptyArray,
            "negative size" => NegativeSize,
            "invalid stream" => InvalidStream,
            
            other => Self::other(other)
        }
    }

    /// Create new [SqVmError::Other] from string
    pub fn other<T>(err_str: T) -> Self where T: Into<String> {
        Self::Other(Some(err_str.into()))
    }

    /// Add received type information for certain errors
    pub fn type_was(mut self, t: SqType) -> Self {
        if let SqVmError::InvalidType { ref mut received, .. } = self {
            *received = Some(t)
        };
        self
    }

    /// Convert vm error into stack error with explanatory message
    pub fn into_stack_error(self, msg: &'static str) -> SqStackError {
        SqStackError { reason: SqStackErrorReason::SqVmError(self), msg }
    }
}

