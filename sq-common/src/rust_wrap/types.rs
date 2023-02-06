use std::cmp::Ordering;
use std::hash::Hash;
use std::fmt::Write;

use indexmap::IndexMap;

use bitflags::bitflags;

use super::api::*;
use super::vm;

/// Safe abstraction for SQFn
pub type SqFnClosure = dyn FnMut(&vm::Vm<vm::safety::Friend>) -> isize + Send; 
pub type SqBoxedClosure = Box<SqFnClosure>;
pub type SqUserPointer<T> = *mut T;
pub type SqFloat = SQFloat;

/// Squirrel associative container
pub type SqTable = IndexMap<DynSqVar, DynSqVar>;

/// Rust-adapted SQObjectType enum
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum SqType {
    Null,
    Integer,
    String,
    Array,
    Table,
    Float,
    Bool,
    UserData,
    Closure,
    NativeClosure,
    Generator,
    UserPointer,
    Thread,
    FuncProto,
    Class,
    Instance,
    WeakRef,
}

impl SqType {
    /// Is type that can contain other types
    pub fn is_complex(&self) -> bool {
        !matches!(self, SqType::Null
            | SqType::Integer
            | SqType::String
            | SqType::Float
            | SqType::Bool
            | SqType::UserPointer
        )
    }

    pub fn is_closure(&self) -> bool {
        matches!(self, SqType::Closure | SqType::NativeClosure)
    }
}


impl From<SQObjectType> for SqType {

    #[allow(non_upper_case_globals)]
    fn from(val: SQObjectType) -> Self {
        match val {
            tagSQObjectType_OT_NULL => SqType::Null,
            tagSQObjectType_OT_INTEGER => SqType::Integer,
            tagSQObjectType_OT_STRING => SqType::String,
            tagSQObjectType_OT_ARRAY => SqType::Array,
            tagSQObjectType_OT_FLOAT => SqType::Float,
            tagSQObjectType_OT_BOOL => SqType::Bool,
            tagSQObjectType_OT_TABLE => SqType::Table,
            tagSQObjectType_OT_USERDATA => SqType::UserData,

            tagSQObjectType_OT_CLOSURE => SqType::Closure,
            tagSQObjectType_OT_NATIVECLOSURE => SqType::NativeClosure,
            tagSQObjectType_OT_GENERATOR => SqType::Generator,
            tagSQObjectType_OT_USERPOINTER => SqType::UserPointer,
            tagSQObjectType_OT_THREAD => SqType::Thread,
            tagSQObjectType_OT_FUNCPROTO => SqType::FuncProto,
            tagSQObjectType_OT_CLASS => SqType::Class,
            tagSQObjectType_OT_INSTANCE => SqType::Instance,
            tagSQObjectType_OT_WEAKREF => SqType::WeakRef,

            _ => unreachable!()
        }
    }
}


bitflags! {
    /// Type-checking mask of squirrel native closure
    pub struct SqTypedArgMask: u32 { 
        const Any = 0xFFFFFFFF;
        const Null = _RT_NULL;
        const Integer = _RT_INTEGER;
        const Float = _RT_FLOAT;
        const String = _RT_STRING;
        const Table = _RT_TABLE;
        const Array = _RT_ARRAY;
        const Userdata = _RT_USERDATA;
        const Closure = _RT_CLOSURE | _RT_NATIVECLOSURE;
        const Bool = _RT_BOOL;
        const Generator = _RT_GENERATOR;
        const Userpointer = _RT_USERPOINTER;
        const Thread = _RT_THREAD;
        const Instance = _RT_INSTANCE;
        const Class = _RT_CLASS;
        const Weakref = _RT_WEAKREF;
    }
}

impl std::fmt::Display for SqTypedArgMask {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_all() {
            write!(f, "Any")
        } else {
            write!(f, "{self:?}")
        }
    }
}


/// Newtype wrapper for getting and pushing userdata
#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub struct SqUserData(pub Vec<u8>);

impl SqUserData {
    pub fn unwrap(self) -> Vec<u8> {
        self.0
    }
}

impl From<Vec<u8>> for SqUserData {
    fn from(value: Vec<u8>) -> Self {
        SqUserData(value)
    }
}

/// Squirrel class isntance
#[derive(Clone, Debug)]
pub struct SqInstance {
    pub this: SqTable
}

#[derive(Clone, Debug)]
pub struct SqClosureInfo {
    pub name: Option<String>,
    pub args: Vec<String>,
    pub src: Option<String>,
}

#[derive(Clone, Debug)]
pub struct SqNativeClosureInfo {
    pub name: Option<String>,
    pub arg_types: Vec<SqTypedArgMask>,
}

#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub struct SqNull;

/// Rust representation of dynamically-typed squirrel variable
#[derive(Clone, Debug)]
pub enum DynSqVar {
    Null,
    Integer(isize),
    Float(SqFloat),
    Bool(bool),
    String(String),
    Table(SqTable),
    Class(SqTable),
    Instance(SqInstance),
    Array(Vec<DynSqVar>),
    UserData(SqUserData),
    UserPointer(SqUserPointer<u8>),
    Closure(SqClosureInfo),
    NativeClosure(SqNativeClosureInfo),
    NotExpanded(SqType)
} 

// Due to userpointer
unsafe impl Send for DynSqVar {}

impl DynSqVar {
    /// Get variable type
    pub fn get_type(&self) -> SqType {
        match self {
            Self::Null => SqType::Null,
            Self::Integer(_) => SqType::Integer,
            Self::Float(_) => SqType::Float,
            Self::Bool(_) => SqType::Bool,
            Self::String(_) => SqType::String,
            Self::Table(_) => SqType::Table,
            Self::Class(_) => SqType::Class,
            Self::Instance(_) => SqType::Instance,
            Self::Array(_) => SqType::Array,
            Self::UserData(_) => SqType::UserData,
            Self::UserPointer(_) => SqType::UserPointer,
            Self::Closure(_) => SqType::Closure,
            Self::NativeClosure(_) => SqType::NativeClosure,
            Self::NotExpanded(t) => *t,
        }
    }

    fn write_spaces(f: &mut std::fmt::Formatter<'_>, spaces: usize) -> std::fmt::Result {
        for _ in 0..spaces {
            f.write_char(' ')?
        }
        Ok(())
    }

    /// Indented pretty-print helper
    fn fmt_indent(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        const INDENT_INC: usize = 4;
        const HEXDUMP_W: usize = 16;
        match self {
            Self::Null => write!(f, "null"),
            Self::Integer(i) => write!(f, "{i}"),
            Self::Float(flt) => write!(f, "{flt}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::String(s) => write!(f, "\"{s}\""),

            Self::Table(map)
            | Self::Class(map)
            | Self::Instance( SqInstance { this: map } ) => {
                match self.get_type() {
                    SqType::Class => write!(f, "class ")?,
                    SqType::Instance => write!(f, "instance ")?,
                    _ => ()
                }

                if map.is_empty() {
                    write!(f, "{{}}")?;
                    return Ok(())
                }
                
                writeln!(f, "{{")?;
                for (key, val) in map {
                    Self::write_spaces(f, indent + INDENT_INC)?;
                    write!(f, "{key} <- ")?;
                    val.fmt_indent(f, indent + INDENT_INC)?;
                    writeln!(f, ",")?;
                }
                Self::write_spaces(f, indent)?;
                write!(f, "}}")?;
                Ok(())
            }

            Self::Array(v) => { 
                if v.is_empty() {
                    write!(f, "[]")?;
                    return Ok(())
                }

                writeln!(f, "[")?;
                for var in v {
                    Self::write_spaces(f, indent + INDENT_INC)?;
                    var.fmt_indent(f, indent + INDENT_INC)?;
                    writeln!(f, ",")?;
                }
                Self::write_spaces(f, indent)?;
                write!(f, "]")?;
                Ok(())
            }
            // Hexdump-like
            Self::UserData(u) => {
                if u.0.is_empty() {
                    write!(f, "[]")?;
                    return Ok(())
                }

                writeln!(f, "[")?;
                for chunk in u.0.chunks(HEXDUMP_W) {
                    Self::write_spaces(f, indent + INDENT_INC)?;

                    for byte in chunk {
                        write!(f, "{byte:02X} ")?;
                    }
                    
                    let skipped = HEXDUMP_W - chunk.len();
                    Self::write_spaces(f, skipped * 3)?;

                    write!(f, "| ")?;

                    for byte in chunk {
                        write!(f, "{}", char::from(*byte))?;
                    }

                    writeln!(f)?;
                }
                Self::write_spaces(f, indent)?;
                write!(f, "]")?;
                Ok(())
            }

            Self::UserPointer(p) => write!(f, "ptr {p:p}"),

            Self::Closure(SqClosureInfo { name, args, .. }) => {
                let name = name.as_deref().unwrap_or("function");
                write!(f, "closure {name}(")?;

                for (idx, arg) in args.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }

                write!(f, ")")
            }

            Self::NativeClosure(SqNativeClosureInfo { name, arg_types }) => {
                let name = name.as_deref().unwrap_or("fn");
                write!(f, "native {name}(")?;
                
                for (idx, arg_type) in arg_types.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg_type}")?;
                }

                write!(f, ")")
            }

            Self::NotExpanded(t) => write!(f, "{t:?}"),
        }
    }
}

impl std::fmt::Display for DynSqVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_indent(f, 0)
    }
}

impl PartialEq for DynSqVar {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Null, Self::Null) => true,
            (Self::Integer(l0), Self::Integer(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Array(l0), Self::Array(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl Eq for DynSqVar {}

impl PartialOrd for DynSqVar {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Null, Self::Null) => Some(Ordering::Equal),
            (Self::Integer(l), Self::Integer(r)) => l.partial_cmp(r),
            (Self::Bool(l), Self::Bool(r)) => l.partial_cmp(r),
            (Self::String(l), Self::String(r)) => l.partial_cmp(r),
            (Self::Array(l), Self::Array(r)) => l.partial_cmp(r),
            _ => None
        }
    }
}

impl Hash for DynSqVar {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Integer(i) => i.hash(state),
            Self::Bool(b) => b.hash(state),
            Self::String(s) => s.hash(state),
            Self::Array(a) => a.hash(state),
            Self::UserData(u) => u.hash(state),
            Self::Null => core::mem::discriminant(self).hash(state),
            _ => unimplemented!()
        }

    }
}

/// Type that is similar to rust's `()` and can be used for same purposes.
///
/// If function or closure with `#[sqfn]` attribute returns `SqUnit`, it will indicate to vm,
/// that return value is on stack top, allowing to push it manually. 
pub struct SqUnit;

