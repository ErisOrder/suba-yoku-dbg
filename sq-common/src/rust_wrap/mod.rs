use squirrel2_kaleido_rs::*;
use util_proc_macro::{set_sqfn_paths, sq_closure};
use std::{ptr::{addr_of_mut, addr_of}, cmp::Ordering, hash::Hash, fmt::Write, marker::PhantomData};
use bitflags::bitflags;

use crate::{raw_api::*, sq_validate};

use crate::error::*;

set_sqfn_paths!(sq_wrap_path = "self");

mod vm;
mod api; 
mod types;
mod get;
mod push;
mod obj;
mod iter;
mod throw;

/// Re-export
pub use crate::raw_api as raw_api;
pub use crate::error as error;
pub use indexmap::IndexMap;
pub use squirrel2_kaleido_rs::HSQUIRRELVM;

/// Copy foreign C str to owned String
///
/// # Safety
/// Safe as long as ptr is to normal null-terminated string
pub unsafe fn cstr_to_string(ptr: *const i8) -> String {
    let len = libc::strlen(ptr as _);
    let mut v = Vec::with_capacity(len);
    std::ptr::copy(ptr, v.as_mut_ptr() as _, len);
    v.set_len(len);
    String::from_utf8_unchecked(v)
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

/// Safe SQFuntionProto structure
#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub struct SqFunctionInfo {
    /// Basically a pointer to function
    pub func_id: *mut libc::c_void,
    pub name: Option<String>,
    pub src_file: Option<String>,
}

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub struct SqStackInfo {
    pub funcname: Option<String>,
    pub src_file: Option<String>,
    pub line: Option<SqInteger>, 
}

impl std::fmt::Display for SqStackInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{src}:{func} ({ln})",
            src = self.src_file.as_deref().unwrap_or("??"),
            func = self.funcname.as_deref().unwrap_or("??"),
            ln = self.line.map_or("??".into(), |l| l.to_string())
        )
    }
}

pub type SqReleaseHook = extern "C" fn(p: SQUserPointer, size: SqInteger) -> SqInteger;

pub type SqCompilerErrorHandler = extern "C" fn(
    vm: HSQUIRRELVM,
    desc: *const i8,
    src: *const i8,
    line: SqInteger,
    column: SqInteger,
);

/// C SQFunction type 
pub type SqFunction = extern "C" fn(HSQUIRRELVM) -> SqInteger;

/// Safe abstraction for SQFn
pub type SqFnClosure = dyn FnMut(&mut FriendVm) -> SqInteger + Send; 

pub type SqBoxedClosure = Box<SqFnClosure>;

/// For naming consistency
pub type SqInteger = SQInteger;

/// For naming consistency
pub type SqUnsignedInteger = SQUnsignedInteger;

/// For naming consistency
pub type SqFloat = SQFloat;

pub type SqUserPointer<T> = *mut T;



/// Event that VM debug hook may receive
#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum DebugEvent {
    /// Linenumber
    Line(SqInteger),
    /// Function name, linenumber
    FnCall(String, Option<SqInteger>),
    /// Function name, linenumber
    FnRet(String, Option<SqInteger>),
}

/// DebugEvent bundled with source path
#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub struct DebugEventWithSrc {
    pub event: DebugEvent,
    pub src: Option<String>
}

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

#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub struct SqNull;

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

/// Safe wrapper around SQVM handle.
/// Will be closed on drop 
pub struct SafeVm {
    handle: HSQUIRRELVM,
}

unsafe impl Send for SafeVm {}

impl VmHandle for SafeVm {
    #[inline]
    fn handle(&self) -> HSQUIRRELVM {
        self.handle
    }
}

impl Drop for SafeVm {
    fn drop(&mut self) {
        unsafe { sq_close(self.handle) }
    }
}

impl SafeVm {
    /// Creates a new instance of a squirrel VM that consists in a new execution stack.
    pub fn open(initial_stack_size: SqInteger) -> Self {
        let handle = unsafe {
             sq_open(initial_stack_size)
        };
        Self { handle }
    }
}

/// Wrapper for manipulating foreign VMs
/// Will not be closed automatically
pub struct UnsafeVm(HSQUIRRELVM);

impl VmHandle for UnsafeVm {
    #[inline]
    fn handle(&self) -> HSQUIRRELVM {
        self.0
    }
}

impl UnsafeVm {
    /// Create struct from raw pointer to vm instance
    /// 
    /// # Safety
    /// Unsafe VM does not closed at drop
    pub unsafe fn from_handle(handle: HSQUIRRELVM) -> Self {
        Self(handle)
    }

    /// Release a squirrel VM and all related friend VMs
    pub fn close(&mut self) {
        unsafe { sq_close(self.0) }
    }

    /// Transform into Safe VM.
    /// Safe Vm will be closed on drop
    pub fn into_safe(self) -> SafeVm {
        SafeVm { handle: self.0 }
    }

    /// Transform into Friend VM.
    /// # Safety
    /// Friend Vm __cannot__ be closed
    pub unsafe fn into_friend(self) -> FriendVm {
        FriendVm(self.0)
    }

}

/// Wrapper for friend SQVM. Cannot be closed by user.
pub struct FriendVm(HSQUIRRELVM);

impl VmHandle for FriendVm {
    #[inline]
    fn handle(&self) -> HSQUIRRELVM {
        self.0
    }
}

/// Strongly-typed vm errors
macro_rules! sq_try {
    ($vm:expr, $e:expr) => {
        {
            let out = $e;
            if out == SQ_ERROR {
                let err = unsafe { $vm.last_error_cstr().map(|c| c.to_str()) }; 
                // TODO: Resolve issue with errors that were left not by this macro...
                $vm.reset_error();
                Err(match err {
                    Some(Ok(err)) => SqVmError::parse(err),
                    Some(Err(e)) => SqVmError::other(e.to_string()),
                    None => SqVmError::Other(None),
                })
            } else { Ok(out) }
        }
    };
    ($vm:expr, nothrow $e:expr) => {
        {
            let out = $e;
            if out == SQ_ERROR {
                Err(SqVmError::Other(None))
            } else {
                Ok(out)
            }
        }
    }
}

/// Main vm trait
pub trait SqVm: VmRawApi {
    /// Get last VM error.
    /// Return `None` if failed to decode string into utf-8. 
    /// # Panics
    /// Panics if failed to get error string.
    #[inline]
    fn last_error(&self) -> Option<String> {
        unsafe { 
            self.last_error_cstr()
                .and_then(|cstr| cstr.to_str().ok().map(|c| c.into())) 
        }
    }

    /// Get last VM error as reference, without copy
    /// # Safety
    /// This function returns reference to string which lifetime 
    /// controlled by SQVM. It may be freed when popped from VM stack.
    /// # Panics
    /// Panics if failed to get error string
    #[inline]
    unsafe fn last_error_cstr(&self) -> Option<&std::ffi::CStr> {
        self.getlasterror();

        match self.get_type(-1) {
            SqType::Null => None,
            SqType::String => {
                let mut ptr = std::ptr::null();
                if self.getstring(-1, addr_of_mut!(ptr)) == SQ_ERROR {
                    panic!("Failed to get last error")
                }
                Some(std::ffi::CStr::from_ptr(ptr))        
            }
            other => panic!("Unknown error type {other:?}"),
        }
    }

    /// Throw error string as an exception to the vm
    #[inline]
    fn throw_error(&self, mut msg: String) {
        msg.push('\0');
        // sq_throwerror copies the string
        unsafe { self.throwerror(msg.as_ptr() as _); }
    }

    /// Creates a new friend vm of this one
    /// and pushes it in its stack as "thread" object.
    #[inline]
    fn new_thread(&self, initial_stack_size: usize) -> FriendVm {
        let handle = unsafe {
            self.newthread(initial_stack_size as _)
        };

        FriendVm(handle)
    }

    /// Pushes the object at the position `idx` of the source vm stack in the destination vm stack
    #[inline]
    fn move_obj(&self, to: &SafeVm, idx: SqInteger) {
        unsafe { self.move_object(to.handle(), idx) }
    }

    /// Compares 2 objects from the stack.
    #[inline]
    fn stack_cmp(&self) -> Ordering {
        match self.stack_compare() {
            1.. => Ordering::Greater,
            0 => Ordering::Equal,
            ..=-1 => Ordering::Less,
        }
    }

    /// Get the type of the value at the position `idx` in the stack
    #[inline]
    fn get_type(&self, idx: SqInteger) -> SqType {
        self.get_obj_type(idx).into()
    }

    /// Creates a new array and pushes it into the stack.
    #[inline]
    fn new_array(&self, size: usize) {
        self.newarray(size as _)
    }

    /// Pops a value from the stack and pushes it in the back
    /// of the array at the position `idx` in the stack.
    #[inline]
    fn array_append(&self, idx: SqInteger) -> SqVmResult<()> {
        sq_try! { self, unsafe { self.arrayappend(idx) } }?;
        Ok(())
    }
    
    /// Pops a key and a value from the stack and performs a set operation
    /// on the table or class that is at position `idx` in the stack,
    /// if the slot does not exits it will be created.
    /// 
    /// if `is_static = true` creates a static member.
    /// This parameter is only used if the target object is a class
    #[inline]
    fn new_slot(&self, idx: SqInteger, is_static: bool) -> SqVmResult<()> {
        sq_try! { self, unsafe { self.newslot(idx, is_static as _) } }?;
        Ok(())
    }

    /// Pops a key and a value from the stack and performs a set operation
    /// on the object at position `idx` in the stack.
    /// 
    /// this call will invoke the delegation system like a normal assignment,
    /// it only works on tables, arrays and userdata.
    #[inline]
    fn slot_set(&self, idx: SqInteger) -> SqVmResult<()> {
        sq_try! { self, self.set_to_slot(idx) }?;
        Ok(())
    }

    /// Pops a key from the stack and performs a get operation on the object 
    /// at the position `idx` in the stack, and pushes the result in the stack.
    /// 
    /// This call will invoke the delegation system like a normal dereference. 
    /// It only works on tables, instances, arrays and classes.
    /// If the function fails nothing will be pushed in the stack.
    #[inline]
    fn slot_get(&self, idx: SqInteger) -> SqVmResult<()> {
        sq_try!{ self, self.get_from_slot(idx) }?;
        Ok(())
    }

    /// Pops a key from the stack and performs a get operation on the object 
    /// at the position `idx` in the stack, and pushes the result in the stack,
    /// without employing delegation or metamethods.
    ///  
    /// It only works on tables, instances, arrays and classes.
    /// If the function fails nothing will be pushed in the stack.
    #[inline]
    fn slot_get_raw(&self, idx: SqInteger) -> SqVmResult<()> {
        sq_try!{ self, unsafe { self.rawget(idx) } }?;
        Ok(())
    }

    /// Pushes in the stack the next key and value of an array, table or class slot. 
    /// 
    /// To start the iteration this function expects a `null` value on top of the stack.
    /// 
    /// At every call the function will substitute the `null` value with an iterator 
    /// of object at `idx` and push key and value of the container slot.
    /// 
    /// Every iteration the application has to pop the previous key and value
    /// but leave the iterator (that is used as reference point for the next iteration).
    /// 
    /// The function will fail when all slots have been iterated
    /// (see Tables and arrays manipulation)
    #[inline]
    fn sq_iter_next(&self, idx: SqInteger) -> Option<()> {
        if unsafe { self.next(idx) } >= 0 {
            Some(())
        } else {
            None
        }
    }

    /// Compiles a squirrel program.
    /// 
    /// if it succeeds, push the compiled script as function in the stack.
    /// 
    /// Args:
    /// - `src_name` - the symbolic name of the program (used only for debugging)
    /// - `raise_err` - if `true`, vm will call compiler error handler
    fn compile_string(&self, script: String, mut src_name: String, raise_err: bool) -> SqVmResult<()> {
        src_name.push('\0');

        sq_try! { self, nothrow unsafe { 
            self.compilebuffer(
                script.as_ptr() as _,
                script.len() as _,
                src_name.as_ptr() as _,
                raise_err as _
            ) 
        }}?;
        Ok(())
    }

    /// Pushes class of a class instance at position `idx`
    #[inline]
    fn get_instance_class(&self, idx: SqInteger) -> SqVmResult<()> {
        sq_try! { self, unsafe { self.getclass(idx) } }?;
        Ok(())
    }

    /// Push info table of closure on stack index `idx` 
    fn get_closure_info(&self, idx: SqInteger) -> SqVmResult<()> {
        sq_try! { self, unsafe { self.closure_getinfos(idx) } }?;
        Ok(())
    }

    /// Pops an object from the stack (must be a table, instance or class) clones
    /// the closure at position `idx` in the stack and sets the popped object
    /// as environment of the cloned closure.
    ///
    /// Then pushes the new cloned closure on top of the stack
    ///
    /// The cloned closure holds the environment object as weak reference.
    fn bind_env(&self, idx: SqInteger) -> SqVmResult<()> {
        sq_try!{ self, unsafe { self.bindenv(idx) } }?;
        Ok(())
    }

    /// Get a pointer to the string at the `idx` position in the stack.
    #[inline]
    fn get_string(&self, idx: SqInteger) -> SqVmResult<*const u8> {
        let mut ptr = std::ptr::null_mut();
        sq_try! { self,
            unsafe { self.getstring(idx, addr_of_mut!(ptr) as _) }
        }?;
        Ok(ptr)
    }

    /// Create a new native closure, pops `free_vars` values and set those
    /// as free variables of the new closure, and push the new closure in the stack
    #[inline]
    fn new_closure(&self, f: SqFunction, free_vars: SqUnsignedInteger) {
        unsafe { self.newclosure(Some(f), free_vars) }
    }

    /// Get the value of the integer at the `idx` position in the stack.
    #[inline]
    fn get_integer(&self, idx: SqInteger) -> SqVmResult<SqInteger> {
        let mut out = 0;
        sq_try! { self, nothrow unsafe { self.getinteger(idx,  addr_of_mut!(out)) }}?;
        Ok(out)
    }

    /// Get the value of the bool at the `idx` position in the stack.
    #[inline]
    fn get_bool(&self, idx: SqInteger) -> SqVmResult<bool> {
        let mut out = 0;
        sq_try! { self, nothrow unsafe { self.getbool(idx, addr_of_mut!(out)) }}?;
        Ok(out != 0)
    }

    /// Gets the value of the float at the idx position in the stack.
    #[inline]
    fn get_float(&self, idx: SqInteger) -> SqVmResult<SqFloat> {
        let mut out = 0.0;
        sq_try! { self, nothrow unsafe { self.getfloat(idx, addr_of_mut!(out)) }}?;
        Ok(out)
    }

    /// Gets a pointer to the value of the userdata at the `idx` position in the stack.
    ///
    /// Returns (`ptr`, `type_tag`)
    /// * `ptr` - userpointer that will point to the userdata's payload
    /// * `type_tag` -  `SQUserPointer` that will store the userdata tag(see sq_settypetag).
    #[inline]
    fn get_userdata(&self, idx: SqInteger) -> SqVmResult<(SQUserPointer, SQUserPointer)> {
        let mut ptr = std::ptr::null_mut();
        let mut typetag = std::ptr::null_mut();
        sq_try! { self, nothrow
            unsafe { self.getuserdata(idx, addr_of_mut!(ptr), addr_of_mut!(typetag)) }
        }?;
        Ok((ptr, typetag))
    }

    /// Get the value of the userpointer at the `idx` position in the stack.
    #[inline]
    fn get_userpointer(&self, idx: SqInteger) -> SqVmResult<SQUserPointer> {
        let mut ptr = std::ptr::null_mut();
        sq_try! { self, nothrow
            unsafe { self.getuserpointer(idx, addr_of_mut!(ptr)) }
        }?;
        Ok(ptr)
    }

    /// Returns the size of a value at the idx position in the stack
    /// Works only for arrays, tables, userdata, and strings
    #[inline]
    fn get_size(&self, idx: SqInteger) -> SqVmResult<SqInteger> {
        sq_try! { self,
            unsafe { self.getsize(idx) }
        }
    }

    /// Sets a `hook` that will be called before release of __userdata__ at position `idx`
    #[inline]
    fn set_release_hook(&self, idx: SqInteger, hook: SqReleaseHook) -> SqVmResult<()> {
        sq_validate!(self.get_type(idx), SqType::UserData)?;
        unsafe { self.setreleasehook(idx, Some(hook)) };
        Ok(())
    }

    /// Call a closure or a native closure.
    ///
    /// the function pops all the parameters and leave the closure in the stack.
    ///
    /// if `retval` is true the return value of the closure is pushed.
    ///
    /// If the execution of the function is suspended through sq_suspendvm(),
    /// the closure and the arguments will not be automatically popped from the stack.
    #[inline]
    fn call_closure_api(&self, params: SqInteger, retval: bool, raise_error: bool) -> SqVmResult<()> {
        sq_try! { self,
            unsafe { self.call(params, retval as _, raise_error as _) }
        }?;
        Ok(())
    }

    /// Set the compiler error handler function.
    ///
    /// The compiler error handler is shared between friend VMs.
    #[inline]
    fn set_compiler_error_handler(&self, handler: Option<SqCompilerErrorHandler>) {
        let handler: SQCOMPILERERROR = handler.map(|h| h as _);
        unsafe { self.setcompilererrorhandler(handler); }
    }

    /// Adds a reference to an object handler.
    fn inc_ref(&self, obj: &mut SQObject) {
        unsafe { self.addref(addr_of_mut!(*obj)) }
    }

    /// Remove a reference from an object handler.
    ///
    /// Returns `true` if object was deleted and if so, resets object handler to null.
    fn dec_ref(&self, obj: &mut SQObject) -> bool {
        (unsafe { self.release(addr_of_mut!(*obj)) }) != 0
    }

    /// Initialize object handler.
    fn obj_init() -> SQObject {
        let mut obj = unsafe { std::mem::zeroed() };
        unsafe { sq_resetobject(addr_of_mut!(obj)) };
        obj
    }

    /// Gets an object (or it's pointer) from the stack and stores it in a object handler.
    fn get_stack_obj(&self, idx: SqInteger) -> SqVmResult<SQObject> {
        let mut obj = Self::obj_init();
        sq_try! { self,
            unsafe { self.getstackobj(idx, addr_of_mut!(obj)) }
        }?;
        Ok(obj)
    }

    /// Push an object referenced by an object handler into the stack.
    fn push_stack_obj(&self, obj: &SQObject) {
        // Looks like sq_pushobject actually reads not object but ptr to it,
        // Despite the function signature. WTF?
        // So this struct's _type field used to pass pointer.
        let mut stub_wtf: SQObject = unsafe { std::mem::zeroed() };
        stub_wtf._type = addr_of!(*obj) as _;
        unsafe { self.pushobject(stub_wtf) }
    }
}

/// Iterator on squirrel array
pub struct SqArrayIter<'vm, VM, T> 
where VM: SqVm {
    vm: &'vm VM,
    max_depth: Option<u32>,
    _type: PhantomData<T>,
}

impl<VM, T> Drop for SqArrayIter<'_, VM, T>
where VM: SqVm {
    fn drop(&mut self) {
        // Pop the null iterator and the reference
        self.vm.pop(2);
    }
}

impl<VM, T> Iterator for SqArrayIter<'_, VM, T>
where 
    VM: SqVm + SqGet<T>
{
    type Item = SqGetResult<T>;

    fn next(&mut self) -> Option<Self::Item> {
        let next_depth = self.max_depth.map(|d| d - 1);
        self.vm.sq_iter_next(-3).map(|_| {
            let elem = self.vm.get_constrain(-1, next_depth);
            // Pop key-val
            if elem.is_ok() {
                self.vm.pop(2);
            }

            elem
        })
    }
}

/// Iterator on squirrel table
pub struct SqTableIter<'vm, VM, K, V> 
where VM: SqVm {
    vm: &'vm VM,
    max_depth: Option<u32>,
    _type: PhantomData<(K, V)>,
}

impl<VM, K, V> Drop for SqTableIter<'_, VM, K, V>
where VM: SqVm {
    fn drop(&mut self) {
        // Pop the null iterator and the reference
        self.vm.pop(2);
    }
}

impl<VM, K, V> Iterator for SqTableIter<'_, VM, K, V>
where 
    VM: SqVm + SqGet<K> + SqGet<V>
{
    type Item = SqGetResult<(K, V)>;

    fn next(&mut self) -> Option<Self::Item> {
        let next_depth = self.max_depth.map(|d| d - 1);
        self.vm.sq_iter_next(-3).map(|_| {
            let val: V = match self.vm.get_constrain(-1, next_depth) {
                Ok(v) => v,
                Err(e) => return Err(e),
            };

            let key: K = match self.vm.get_constrain(-2, next_depth) {
                Ok(k) => k,
                Err(e) => return Err(e),
            };

            // Pop key-val
            self.vm.pop(2);
            Ok((key, val))
        })
    }
}

/// In-place squirrel iterators interface
pub trait SqVmIterators<'vm>: SqVm where Self: Sized {

    /// Get rust iterator to squirrel array at index `idx`
    fn iter_array<T>(
        &'vm self,
        idx: SqInteger,
        max_depth: Option<u32>
    ) -> SqArrayIter<'vm, Self, T> {
        // Push a reference to an array to the stack top and a null iterator
        self.ref_idx(idx);
        self.push_null();

        SqArrayIter { vm: self, max_depth, _type: PhantomData }
    }

    /// Get rust iterator to squirrel table at index `idx`
    fn iter_table<K, V>(
        &'vm self,
        idx: SqInteger,
        max_depth: Option<u32>
    ) -> SqTableIter<'vm, Self, K, V> {
        // Push a reference to an array to the stack top and a null iterator
        self.ref_idx(idx);
        self.push_null();

        SqTableIter { vm: self, max_depth, _type: PhantomData }
    }
}

/// SQVM  local variable
#[derive(Clone, PartialEq, PartialOrd, Eq, Debug, Hash)]
pub struct SqLocalVar {
    pub name: String,
    pub val: DynSqVar,
}

/// SQVM local variable handle
pub struct SqLocalVarHandle<'vm, VM>
where VM: SqVm {
    pub name: String,
    pub handle: SqObjectRef<'vm, VM>,
}

/// Advanced rust-wrapped operations (ones that depend on SqGet, SqPush, etc)
pub trait SqVmAdvanced<'a>:
    SqVm + SqPush<&'a str> + SqPush<SqFunction> + SqGet<SqUserData>
    + SqGet<DynSqVar> + Sized + SqPush<SqBoxedClosure> + SqPush<SqUserData>
{

    /// Set VM debug hook that will be called for each line and function call/return
    /// 
    /// In order to receive a 'per line' callback, is necessary 
    /// to compile the scripts with the line informations. 
    /// Without line informations activated, only the 'call/return' callbacks will be invoked.
    fn set_debug_hook<F>(&mut self, mut hook: F)
    where
        F: FnMut(DebugEventWithSrc, &FriendVm) + Send + 'static
    {

        let debug_hook_glue = sq_closure!(
            #[(vm_var = "vm")]
            move |
            event_type: SqInteger,
            src: Option<String>,
            line: SqInteger,
            funcname: Option<String>
            | {
                let line_opt = if line > 0 { Some(line) } else { None };

                let event = match char::from_u32(event_type as u32).unwrap() {
                    'l' => DebugEvent::Line(line),
                    'c' => DebugEvent::FnCall(funcname.unwrap_or_else(|| "??".into()), line_opt),
                    'r' => DebugEvent::FnRet(funcname.unwrap_or_else(|| "??".into()), line_opt),
                    e => panic!("unknown debug event: {e}"),
                };

                hook(DebugEventWithSrc { event, src }, vm);
            }
        );

        <Self as SqPush<SqBoxedClosure>>::push(self, debug_hook_glue);

        unsafe {
            self.setdebughook();
        }
    }

    /// The member 'func_id' of the returned SqFunctionInfo structure is a
    /// unique identifier of the function; this can be useful to identify
    /// a specific piece of squirrel code in an application like for instance a profiler.
    /// this method will fail if the closure in the stack is a native C closure.
    /// 
    /// NOTE: Feels like legacy version of `get_stack_info`
    fn get_function_info(&self, level: SqInteger) -> SqVmResult<SqFunctionInfo> {
        let mut func_info = unsafe { std::mem::zeroed() };

        sq_try! { self,
            unsafe { sq_getfunctioninfo(self.handle(), level, addr_of_mut!(func_info)) }
        }?;

        let name = unsafe { cstr_to_string(func_info.name) };
        let src = unsafe { cstr_to_string(func_info.source) };

        Ok(SqFunctionInfo {
            func_id: func_info.funcid,
            name: if name != "unknown" { Some(name) } else { None },
            src_file: if src != "unknown" { Some(src) } else { None },
        })
    }

    /// Retrieve the call stack informations of a certain `level` in the calls stack
    fn get_stack_info(&self, level: SqUnsignedInteger) -> SqVmResult<SqStackInfo> {
        let mut stack_info = unsafe { std::mem::zeroed() };

        sq_try! { self, nothrow
            unsafe { sq_stackinfos(self.handle(), level as _, addr_of_mut!(stack_info)) }
        }?;

        let name = unsafe { cstr_to_string(stack_info.funcname) };
        let src = unsafe { cstr_to_string(stack_info.source) };

        Ok(SqStackInfo { 
            funcname: if name != "unknown" { Some(name) } else { None },
            src_file: if src != "NATIVE" { Some(src) } else { None },
            line: if stack_info.line > 0 { Some(stack_info.line) } else { None }
        })
    }

    /// Returns the name and value of a local variable given
    /// stackframe and sequence in the stack.
    /// 
    /// Free variables are treated as local variables, and will be returned
    /// as they would be at the base of the stack, just before the real local variable
    /// 
    /// Returns `None` if local on specified `idx` and `level` doesn't exist 
    fn get_local(
            &self,
            level: SqUnsignedInteger,
            idx: SqUnsignedInteger,
            max_depth: Option<u32>,
            ) -> SqGetResult<Option<SqLocalVar>> {
        let ptr = unsafe { sq_getlocal(self.handle(), level, idx) };
        if ptr != 0 as _ {
            let name = unsafe { cstr_to_string(ptr) };
            let val = self.get_constrain(-1, max_depth)?;
            self.pop(1);
            Ok(Some(SqLocalVar{ name, val }))
        } else {
            Ok(None)
        }
    }

    /// Returns the name and handle of a local variable given
    /// stackframe and sequence in the stack.
    ///
    /// Free variables are treated as local variables, and will be returned
    /// as they would be at the base of the stack, just before the real local variable
    ///
    /// Returns `None` if local on specified `idx` and `level` doesn't exist
    fn get_local_handle(
            &self,
            level: SqUnsignedInteger,
            idx: SqUnsignedInteger
    ) -> SqGetResult<Option<SqLocalVarHandle<'_, Self>>> {
        let ptr = unsafe { sq_getlocal(self.handle(), level, idx) };
        if ptr != 0 as _ {
            let name = unsafe { cstr_to_string(ptr) };
            let val = SqObjectRef::get(self, -1)?;
            self.pop(1);
            Ok(Some(SqLocalVarHandle { name, handle: val }))
        } else {
            Ok(None)
        }
    }

    // TODO: Add typemask
    
    /// Bind rust native function to root table of SQVM
    fn register_function(&self, name: &'a str, func: SqFunction) {
        self.push_root_table();
        self.push(name);
        self.push(func);
        self.new_slot(-3, false).expect("Failed to create slot in root table");
        self.pop(1);
    }

    /// Bind rust native closure to root table of SQVM 
    fn register_closure(&self, name: &'a str, closure: Box<SqFnClosure>) {
        self.push_root_table();
        self.push(name);
        self.push(closure);
        self.new_slot(-3, false).expect("Failed to create slot in root table");
        self.pop(1);
    }

    /// Compile and arbitrary squirrel script
    fn compile_closure(&self, script: String, src_file: String) -> SqCompilerResult<()> {

        /// This handler will push SqCompilerError ptr to stack as userdata
        extern "C" fn error_handler(
            vm: HSQUIRRELVM,
            desc: *const i8,
            src: *const i8,
            line: SqInteger,
            column: SqInteger,
        ) {
            unsafe { 
                let err = Box::new(SqCompilerError::CompileError {
                    line,
                    column,
                    description: cstr_to_string(desc),
                    src_file: cstr_to_string(src),
                });

                let ptr = Box::leak(err) as *mut SqCompilerError;
                UnsafeVm::from_handle(vm).push(ptr);
            }
        }

        self.enable_debug_info(true);    
        self.set_compiler_error_handler(Some(error_handler));
        let compile_res = self.compile_string(script, src_file, true);
        self.set_compiler_error_handler(None);

        if compile_res.is_err() {
            let err_box: SqUserPointer<SqCompilerError> = self.get(-1)?;

            let err: Box<SqCompilerError> = unsafe {
                Box::from_raw(err_box)
            };

            // Pop error
            self.pop(1);

            return Err(*err);
        };
        Ok(())
    }

    /// Call closure with specified argument count and return result.
    ///
    /// `depth` is depth of eager return value containers expansion.
    ///
    /// Returns [SqNull] if closure does not return anything.
    fn closure_call(&self, argc: SqInteger, depth: Option<SqUnsignedInteger>) -> SqGetResult<DynSqVar> {
        self.call_closure_api(argc, true, false)
            .map_err(|e| e.into_stack_error("failed to call closure"))?;
        let ret = self.get_constrain(-1, depth)?;

        // Pop retval
        self.pop(1);
        Ok(ret)
    }
}

impl<T: VmRawApi> SqVm for T {}
impl<T: SqVm> SqVmIterators<'_> for T {}
impl<'a, T> SqVmAdvanced<'a> for T
where 
    T: SqVm + SqPush<&'a str> + SqPush<SqFunction>
{}

pub type SqGetResult<T> = Result<T, SqStackError>;
pub type SqPushResult = Result<(), SqStackError>;
/// Helper trait that defines shared behaviour 
/// for implementing fallible [SqPush].
pub trait IntoPushResult { 
    /// Convert struct to Result
    fn into_result(self) -> SqPushResult;
    fn default_self() -> Self;
}

impl IntoPushResult for () {
    /// Infallible
    fn into_result(self) -> SqPushResult {
        Ok(())
    }

    fn default_self() -> Self {}
}

impl IntoPushResult for Result<(), SqStackError> {
    /// Just pass through
    fn into_result(self) -> SqPushResult {
        self
    }

    fn default_self() -> Self {
        Ok(())
    }
} 

/// Trait for pushing values to vm stack
pub trait SqPush<T> {
    /// Most [SqPush] implementations are infallible,
    /// so we need this associated type for more complex cases
    /// like rust containers
    type Output: IntoPushResult;
    
    /// Push a value to the vm stack
    fn push(&self, val: T) -> Self::Output;
}

/// Trait for getting values from the vm stack
pub trait SqGet<T> {
    /// Get value from the vm stack at position `idx`.
    /// 
    /// if `idx` < 0, count from the top, else from the stack bottom.
    /// 
    /// Limit containers' recursion with `max_depth`.
    /// 
    /// This method mainly exists to prevent eternal recursion of self-referential containers.
    fn get_constrain(&self, idx: SqInteger, max_depth: Option<u32>) -> SqGetResult<T>;
     
    /// Get value from the vm stack at position `idx`
    /// 
    /// if `idx` < 0, count from the top, else from the stack bottom
    /// 
    /// Do not limit recursion
    fn get(&self, idx: SqInteger) -> SqGetResult<T> {
        self.get_constrain(idx, None)
    }
}

/// Trait for throwing errors to vm
pub trait SqThrow<T> {
    /// Throw as a SQ exception to the vm
    fn throw(&self, throwable: T);
}

impl<T: SqVm> SqThrow<SqNativeClosureError> for T {
    fn throw(&self, throwable: SqNativeClosureError) {
        self.throw_error(throwable.to_string())
    }
}

impl<T:SqVm> SqThrow<SqStackError> for T {
    fn throw(&self, throwable: SqStackError) {
        self.throw_error(throwable.to_string())
    }
}

impl<T: SqVm> SqThrow<anyhow::Error> for T {
    fn throw(&self, throwable: anyhow::Error) {
        let msg = throwable.chain()
            .fold(String::new(), |mut msg, e| {
                msg += "\nerror: ";
                msg += &e.to_string();
                msg
            });
        self.throw_error(msg);
    }
}

impl<VM> SqPush<SqBoxedClosure> for VM
where VM: SqPush<SqUserData> + SqVm
{
    type Output = ();
    
    fn push(&self, val: SqBoxedClosure) {
        // Indirection to make fat pointer usable through FFI
        let clos_box = Box::new(val); 

        #[allow(clippy::borrowed_box)]
        extern "C" fn glue(hvm: HSQUIRRELVM) -> SqInteger {
            let mut vm = unsafe { UnsafeVm::from_handle(hvm).into_friend() };

            let top = vm.stack_top();
            let closure_box: SqUserData = vm.get(top).expect("Failed to get closure box ptr");

            let closure: &mut SqBoxedClosure = unsafe {
                let closure_box: usize =  std::ptr::read(closure_box.unwrap().as_ptr() as _) ;
                &mut *(closure_box as *mut _)
            };

            closure(&mut vm)
        }

        extern "C" fn release_hook(ptr: SQUserPointer, _: SqInteger) -> SqInteger {
            // Received ptr is pointer to pointer (of former box) to box
            unsafe { 
                let closure_box: *mut SqBoxedClosure = std::ptr::read(ptr as _) ;
                let _ = Box::from_raw(closure_box);
            };
            1
        }

        let raw = Box::leak(clos_box) as *mut SqBoxedClosure;
        let data: Vec<_> = (raw as usize).to_ne_bytes().into();

        self.push(SqUserData::from(data));
        self.set_release_hook(-1, release_hook).expect("Failed to set box release hook");
        self.new_closure(glue, 1);
    }
}

impl<T: SqVm> SqPush<SqFunction> for T {
    type Output = ();
    
    #[inline]
    fn push(&self, val: SqFunction) {
        self.new_closure(val, 0)
    }
}

impl<T: SqVm> SqPush<SqInteger> for T {
    type Output = ();
    
    #[inline]
    fn push(&self, val: SqInteger) {
        self.push_integer(val);
    }
}

impl<T: SqVm> SqGet<SqInteger> for T {
    #[inline]
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> SqGetResult<SqInteger> {
        sq_validate!(self.get_type(idx), SqType::Integer)
            .map_err(|e| e.into_stack_error("failed to get integer"))?;
        Ok(self.get_integer(idx).unwrap())
    }
}

impl<T: SqVm> SqPush<String> for T {
    type Output = ();
    
    #[inline]
    fn push(&self, val: String) {
        unsafe { self.push_string(val.as_ptr(), val.len() as _) }
    }
}

impl<T: SqVm> SqPush<&str> for T {
    type Output = ();
    
    #[inline]
    fn push(&self, val: &str) {
        unsafe { self.push_string(val.as_ptr(), val.len() as _) }
    }
}

// TODO: Check encoding
// TODO: Use get_size() to make this more safe
impl<T: SqVm> SqGet<String> for T {
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> SqGetResult<String> {
        sq_validate!(self.get_type(idx), SqType::String)
            .map_err(|e| e.into_stack_error("failed to get string"))?;
        unsafe {
            let ptr = self.get_string(idx).unwrap();  
            Ok(cstr_to_string(ptr as _))
        }
    }
}

/// Just for abstraction...
impl<T: SqVm> SqPush<SqNull> for T {
    type Output = ();
    
    #[inline]
    fn push(&self, _: SqNull) {
        self.push_null();
    }
}

/// For type safety
impl<T: SqVm> SqGet<SqNull> for T {
    #[inline]
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> SqGetResult<SqNull> {
        sq_validate!(self.get_type(idx), SqType::Null)
            .map_err(|e| e.into_stack_error("failed to get null"))?;
        Ok(SqNull)
    }
}

impl<VM, T> SqPush<Vec<T>> for VM
where 
    VM: SqPush<T> + SqPush<SqInteger> + SqVm
{
    type Output = SqPushResult; 
    
    fn push(&self, val: Vec<T>) -> Self::Output {
        self.new_array(val.len());

        for (index, elem) in val.into_iter().enumerate() {
            self.push(index as SqInteger);
            self.push(elem).into_result()?;
            self.slot_set(-3)
                .map_err(|e| e.into_stack_error("failed to set array slot"))?;
        }
        Ok(())
    }
}

impl<VM, T> SqGet<Vec<T>> for VM
where 
    VM: SqGet<T> + SqVm
{
    fn get_constrain(&self, idx: SqInteger, max_depth: Option<u32>) -> SqGetResult<Vec<T>> {
        sq_validate!(self.get_type(idx), SqType::Array)
            .map_err(|e| e.into_stack_error("failed to get array"))?;
        
        if matches!(max_depth, Some(depth) if depth == 0) {
            return Ok(vec![]);
        }

        let mut out = vec![];
        
        for elem in self.iter_array(idx, max_depth) {
            out.push(elem?);
        }

        Ok(out)
    }
}

impl<VM, K, V> SqPush<IndexMap<K, V>> for VM
where 
    VM: SqPush<K> + SqPush<V> + SqVm,
{
    type Output = SqPushResult;
    
    fn push(&self, val: IndexMap<K, V>) -> Self::Output {
        self.new_table();

        for (key, val) in val.into_iter() {
            self.push(key).into_result()?;
            self.push(val).into_result()?;

            self.new_slot(-3, false)
                .map_err(|e| e.into_stack_error("failed to set table slot"))?;
        }
        Ok(())
    }
}

impl<VM, K, V> SqGet<IndexMap<K, V>> for VM
where 
    VM: SqGet<K> + SqGet<V> + SqVm,
    K: PartialEq + Eq + Hash,
{
    fn get_constrain(
        &self,
        idx: SqInteger, 
        max_depth: Option<u32>
    ) -> SqGetResult<IndexMap<K, V>> {
        sq_validate!(self.get_type(idx), SqType::Table, SqType::Class)
            .map_err(|e| e.into_stack_error("failed to get table"))?;
        
        if matches!(max_depth, Some(depth) if depth == 0) {
            return Ok(IndexMap::new());
        }

        let mut out = IndexMap::new();

        for pair in self.iter_table(idx, max_depth) {
            match pair {
                Ok((k, v)) => {
                    out.insert(k, v);
                },
                Err(e) => return Err(e),
            } 
        }

        Ok(out)
    }
}

impl<T: SqVm> SqPush<SqFloat> for T {
    type Output = ();
    
    #[inline]
    fn push(&self, val: SqFloat) {
        self.push_float(val);
    }
}

impl<T: SqVm> SqGet<SqFloat> for T {
    #[inline]
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> SqGetResult<SqFloat> {
        sq_validate!(self.get_type(idx), SqType::Float)
            .map_err(|e| e.into_stack_error("failed to get float"))?;
        Ok(self.get_float(idx).unwrap())
    }
}

impl<T: SqVm> SqPush<bool> for T {
    type Output = ();
    
    #[inline]
    fn push(&self, val: bool) {
        self.push_bool(val);
    }
}

impl<T: SqVm> SqGet<bool> for T {
    #[inline]
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> SqGetResult<bool> {
        sq_validate!(self.get_type(idx), SqType::Bool)
            .map_err(|e| e.into_stack_error("failed to get bool"))?;
        Ok(self.get_bool(idx).unwrap())
    }
}


impl<T: SqVm> SqPush<SqUserData> for T {
    type Output = ();
    
    #[inline]
    fn push(&self, val: SqUserData) {
        let val = val.unwrap();
        unsafe { 
            let ptr = self.new_userdata(val.len() as _);
            std::ptr::copy(val.as_ptr() as _, ptr, val.len());
        }
    }
}

impl<T: SqVm> SqGet<SqUserData> for T {
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> SqGetResult<SqUserData> {
        sq_validate!(self.get_type(idx), SqType::UserData)
            .map_err(|e| e.into_stack_error("failed to get userdata"))?;
        
        // TODO: Add typetag to UserData struct
        let (user_data, _) = self.get_userdata(idx).unwrap(); 
        let out = unsafe {
            let size = self.get_size(idx)
                .map_err(|e| e.into_stack_error("failed to get size of userdata"))?
                 as usize;
            let mut out = Vec::with_capacity(size);
            std::ptr::copy(user_data, out.as_ptr() as _, size);
            out.set_len(size);
            SqUserData(out)
        };
        Ok(out)
    }
}


impl<VM, T> SqPush<Option<T>> for VM 
where 
    VM: SqPush<T> + SqVm,
{
    /// Fallible for fallible inner types, infallible otherwise
    type Output = <VM as SqPush<T>>::Output;
    
    fn push(&self, val: Option<T>) -> Self::Output {
        match val {
            Some(v) => self.push(v),
            None => {
                SqPush::<SqNull>::push(self, SqNull);
                Self::Output::default_self()     
            },
        }
    }
}

impl<VM, T> SqGet<Option<T>> for VM 
where
    VM: SqGet<T> + SqVm
{
    fn get_constrain(&self, idx: SqInteger, max_depth: Option<u32>) -> SqGetResult<Option<T>> {
        match self.get_type(idx) {
            SqType::Null => Ok(None),
            _ => Ok(Some(SqGet::<T>::get_constrain(self, idx, max_depth)?))
        }
    }
}

pub type SqTable = IndexMap<DynSqVar, DynSqVar>;

#[derive(Clone, Debug)]
pub struct SqInstance {
    pub this: SqTable
}

impl<VM> SqGet<SqInstance> for VM
where 
    VM: SqVm + SqGet<SqTable> + SqPush<DynSqVar> + SqGet<DynSqVar>
{
    fn get_constrain(&self, idx: SqInteger, max_depth: Option<u32>) -> SqGetResult<SqInstance> {
        sq_validate!(self.get_type(idx), SqType::Instance)
            .map_err(|e| e.into_stack_error("failed to get instance"))?;
        
        if matches!(max_depth, Some(depth) if depth == 0) {
            return Ok(SqInstance { this: IndexMap::new() });
        }

        // Get instance class table with keys and default values
        self.get_instance_class(idx)
            .map_err(|e| e.into_stack_error("failed to get instance class"))?;
        let proto: DynSqVar = self.get_constrain(-1, Some(1))?;

        let DynSqVar::Class(mut proto) = proto else { unreachable!("not a class") };
        // Pop class
        self.pop(1);

        let next_depth = max_depth.map(|d| d - 1);
        for (key, val) in &mut proto {
            // Push class field/method key and get instance value
            self.push(key.clone()).into_result()?;
            self.slot_get_raw(idx - idx.is_negative() as i32)
                .map_err(|e| e.into_stack_error("failed to get slot of instance"))?;

            *val = self.get_constrain(-1, next_depth)?;

            // Clear stack
            self.pop(1);
        }

        Ok(SqInstance { this: proto })
    }
}

#[derive(Clone, Debug)]
pub struct SqClosureInfo {
    name: Option<String>,
    args: Vec<String>,
    src: Option<String>,
}

impl<VM> SqGet<SqClosureInfo> for VM
where 
    VM: SqVm + SqGet<SqTable> + SqGet<DynSqVar>
{
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> SqGetResult<SqClosureInfo> {
        let mut info = SqClosureInfo { name: None, args: vec![], src: None };

        self.get_closure_info(idx)
            .map_err(|e| e.into_stack_error("failed to get closure info"))?;

        // Parameters are 1d array of strings
        for pair in self.iter_table(-1, Some(2)) {
            let (key, val): (String, DynSqVar) = match pair {
                Ok(kv) => kv,
                Err(e) => return Err(e),
            };
            match key.as_str() {
                "name" => if let DynSqVar::String(n) = val {
                    info.name = Some(n);
                },
                "parameters" => if let DynSqVar::Array(v) = val {
                    for arg in v {
                        let DynSqVar::String(arg) = arg else { unreachable!() };
                        info.args.push(arg)
                    }
                }
                "src" => if let DynSqVar::String(n_src) = val {
                    info.src = Some(n_src)
                }
                _ => ()
            }
        }

        // Pop closure info
        self.pop(1);

        Ok(info)
    }
}

#[derive(Clone, Debug)]
pub struct SqNativeClosureInfo {
    name: Option<String>,
    arg_types: Vec<SqTypedArgMask>,
}

impl<VM> SqGet<SqNativeClosureInfo> for VM
where 
    VM: SqVm + SqGet<SqTable> + SqGet<DynSqVar>
{
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> SqGetResult<SqNativeClosureInfo> {
        let mut info = SqNativeClosureInfo { name: None, arg_types: vec![] };

        self.get_closure_info(idx)
            .map_err(|e| e.into_stack_error("failed to get native closure info"))?;

        // Argument types are 1d array of integers
        for pair in self.iter_table(-1, Some(2)) {
            let (key, val): (String, DynSqVar) = match pair {
                Ok(kv) => kv,
                Err(e) => return Err(e),
            };
            match key.as_str() {
                "name" => if let DynSqVar::String(n) = val {
                    info.name = Some(n);
                },
                // Fill vector with Any to represent untyped argument
                "paramscheck" => if let DynSqVar::Integer(nparams) = val {
                    for _ in 0..nparams {
                        info.arg_types.push(SqTypedArgMask::Any);
                    }
                }
                "typecheck" => if let DynSqVar::Array(v) = val {
                    for (idx, mask) in v.into_iter().enumerate() {
                        let DynSqVar::Integer(mask) = mask else { unreachable!() };
                        let mask = SqTypedArgMask::from_bits_truncate(mask as u32);
                        if idx >= info.arg_types.len() {
                            info.arg_types.push(mask)
                        } else {
                            info.arg_types[idx] = mask; 
                        }
                    }
                }
                _ => ()
            }
        }

        // Pop closure info
        self.pop(1);
        
        Ok(info)
    }
}

impl<VM, T> SqGet<SqUserPointer<T>> for VM 
where VM: SqVm
{
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> SqGetResult<SqUserPointer<T>> {
        sq_validate!(self.get_type(idx), SqType::UserPointer)
            .map_err(|e| e.into_stack_error("failed to get userpointer"))?;
        
        Ok(self.get_userpointer(idx).map(|p| p as *mut T).unwrap())
    }
}

impl<VM, T> SqPush<SqUserPointer<T>> for VM 
where VM: SqVm
{
    type Output = ();
    
    fn push(&self, val: SqUserPointer<T>) {
        unsafe { self.push_userpointer(val as _) };
    }
}

#[derive(Clone, Debug)]
pub enum DynSqVar {
    Null,
    Integer(SqInteger),
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


impl<VM> SqPush<DynSqVar> for VM
where 
    VM: SqVm + SqGet<SqNull> // TODO: Why this is working?
    
{
    type Output = SqPushResult;

    #[allow(clippy::unit_arg)]    
    fn push(&self, val: DynSqVar) -> Self::Output {
        match val {
            DynSqVar::Null => self.push(SqNull).into_result(),
            DynSqVar::Integer(i) => self.push(i).into_result(),
            DynSqVar::String(s) => self.push(s).into_result(),
            DynSqVar::Array(v) => self.push(v).into_result(),
            DynSqVar::Float(f) => self.push(f).into_result(),
            DynSqVar::Bool(b) => self.push(b).into_result(),
            DynSqVar::Table(t) => self.push(t).into_result(),
            DynSqVar::UserData(u) => self.push(u).into_result(),
            _ => unimplemented!(),
        }
    }
}



impl<VM> SqGet<DynSqVar> for VM
where 
    VM: SqVm + SqGet<SqNull> + SqGet<SQInteger>
{
    fn get_constrain(&self, idx: SqInteger, max_depth: Option<u32>) -> SqGetResult<DynSqVar> {
        let sq_type = self.get_type(idx);

        // If container, do not expand
        if matches!(max_depth, Some(depth) 
            if depth == 0 && sq_type.is_complex() && !sq_type.is_closure()
        ) {
            return Ok(DynSqVar::NotExpanded(sq_type))
        }

        // Otherwise just pass max_depth without changes
        match self.get_type(idx) {
            SqType::Null => Ok(DynSqVar::Null),
            SqType::Integer => Ok(DynSqVar::Integer(self.get_constrain(idx, max_depth)?)),
            SqType::String => Ok(DynSqVar::String(self.get_constrain(idx, max_depth)?)),
            SqType::Table => Ok(DynSqVar::Table(self.get_constrain(idx, max_depth)?)),
            SqType::Class => Ok(DynSqVar::Class(self.get_constrain(idx, max_depth)?)),
            SqType::Instance => Ok(DynSqVar::Instance(self.get_constrain(idx, max_depth)?)),
            SqType::Array => Ok(DynSqVar::Array(self.get_constrain(idx, max_depth)?)),
            SqType::Float => Ok(DynSqVar::Float(self.get_constrain(idx, max_depth)?)),
            SqType::Bool => Ok(DynSqVar::Bool(self.get_constrain(idx, max_depth)?)),
            SqType::UserData => Ok(DynSqVar::UserData(self.get_constrain(idx, max_depth)?)),
            SqType::UserPointer => Ok(DynSqVar::UserPointer(self.get_constrain(idx, max_depth)?)),
            SqType::Closure => Ok(DynSqVar::Closure(self.get_constrain(idx, max_depth)?)),
            SqType::NativeClosure => Ok(DynSqVar::NativeClosure(self.get_constrain(idx, max_depth)?)),
            other => Ok(DynSqVar::NotExpanded(other)),
        }
    }
}

/// Type that is similar to rust's `()` and can be used for same purposes.
///
/// If function or closure with `#[sqfn]` attribute returns `SqUnit`, it will indicate to vm,
/// that return value is on stack top, allowing to push it manually. 
pub struct SqUnit;

impl<VM> SqGet<SqUnit> for VM 
where VM: VmRawApi {
    fn get_constrain(&self, _: SqInteger, _: Option<u32>) -> SqGetResult<SqUnit> {
        Ok(SqUnit)
    }
}

impl <VM> SqPush<SqUnit> for VM
where VM: VmRawApi {
    type Output = ();
    
    fn push(&self, _: SqUnit) {}
}


/// Strong reference to squirrel vm object with RAII
pub struct SqObjectRef<'vm, VM>
where VM: SqVm + 'vm
{
    obj: SQObject,
    vm: &'vm VM
}

impl<'vm, VM> Drop for SqObjectRef<'vm, VM>
where VM: SqVm {
    fn drop(&mut self) {
        self.vm.dec_ref(&mut self.obj);
    }
}

impl<'vm, VM> SqObjectRef<'vm, VM>
where VM: SqVm {
    /// Get type of referenced object
    pub fn get_type(&self) -> SqType {
        self.obj._type.into()
    }

    /// Get a reference to object on the stack
    pub fn get(vm: &'vm VM, idx: SqInteger) -> SqGetResult<Self> {
        let mut obj = vm.get_stack_obj(idx)
            .map_err(|e| e.into_stack_error("failed to get object handle"))?;
        vm.inc_ref(&mut obj);
        Ok(SqObjectRef {
            obj,
            vm,
        })
    }
}

// TODO: Implementing SqGet for SqObjectRef is possible, but requires
// changing `&self` in SqGet methods to `self` and implementing this trait not for VM,
// but for &VM due to lifetime elision that assumes 
//
// get(&'1 self, ..) -> SqObjectRef<'2, ..>
//       ^                           ^
//
// while what we really need is 
// get(&'1 self, ..) -> SqObjectRef<'1, ..>
//       ^                           ^ 

impl<VM> SqPush<SqObjectRef<'_, VM>> for VM
where VM: SqVm {
    type Output = ();
    
    fn push(&self, val: SqObjectRef<'_, VM>) {
        self.push_stack_obj(&val.obj);
    }
}