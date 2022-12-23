use anyhow::{bail, Context};
use squirrel2_kaleido_rs::*;
use util_proc_macro::{set_sqfn_paths, sq_closure};
use std::{ptr::{addr_of_mut, addr_of}, cmp::Ordering, hash::Hash, fmt::Write, marker::PhantomData};
use bitflags::bitflags;
use anyhow::{
    Result,
    anyhow
};

set_sqfn_paths!(sq_wrap_path = "self");

/// Re-export
pub use indexmap::IndexMap;
pub use squirrel2_kaleido_rs::HSQUIRRELVM;

/// Copy foreign C str to owned String
///
/// # Safety
/// Safe as long as ptr is to normal null-terminated string
pub unsafe  fn cstr_to_string(ptr: *const i8) -> String {
    let len = libc::strlen(ptr as _);
    let mut v = Vec::with_capacity(len);
    std::ptr::copy(ptr, v.as_mut_ptr() as _, len);
    v.set_len(len);
    String::from_utf8_unchecked(v)
}

macro_rules! sq_try {
    ($vm:expr, $e:expr) => {
        {
            let out = $e;
            if out == -1 {
                Err(anyhow!($vm.last_error()))
            } else { Ok(out) }
        }
    };
    ($vm:expr, nothrow $e:expr) => {
        {
            let out = $e;
            if out == -1 {
                Err(anyhow!("unknown error"))
            } else { Ok(out) }
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

/// Error received from SQ compiler
#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
#[repr(C)]
pub struct SqCompilerError {
    pub line: SqInteger,
    pub column: SqInteger,
    pub description: String,
    pub source: String,
}

impl std::fmt::Display for SqCompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let SqCompilerError { line, column, description, source } = self;
        write!(f, "{source}:{line}:{column}: error: {description}")
    }
}

impl std::error::Error for SqCompilerError {}

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

/// Allows to get vm handle from struct
pub trait SqVmHandle {
    /// Expose handle to underlying VM
    /// 
    /// # Safety
    /// Leaks pointer to vm, but does not consume it.
    unsafe fn handle(&self) -> HSQUIRRELVM;
}

/// Safe wrapper around SQVM handle.
/// Will be closed on drop 
pub struct SafeVm {
    handle: HSQUIRRELVM,
}

unsafe impl Send for SafeVm {}

impl SqVmHandle for SafeVm {
    #[inline]
    unsafe fn handle(&self) -> HSQUIRRELVM {
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

impl SqVmHandle for UnsafeVm {
    #[inline]
    unsafe fn handle(&self) -> HSQUIRRELVM {
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

impl SqVmHandle for FriendVm {
    #[inline]
    unsafe fn handle(&self) -> HSQUIRRELVM {
        self.0
    }
}

pub enum SqExecState {
    Idle,
    Running,
    Suspended,
}

impl From<SqInteger> for SqExecState {
    fn from(value: SqInteger) -> Self {
        match value as u32 {
            SQ_VMSTATE_IDLE => Self::Idle,
            SQ_VMSTATE_RUNNING => Self::Running,
            SQ_VMSTATE_SUSPENDED => Self::Suspended,
            _ => unreachable!(),
        }
    }
}

// TODO: Somehow remove cyclic dependency...
/// Getting and throwing VM errors
pub trait SqVmErrorHandling: SqVmHandle + SqGet<String> {
    /// Get last VM error
    #[inline]
    fn last_error(&self) -> String {
        unsafe { sq_getlasterror(self.handle()) }
        self.get(-1).expect("Failed to get last error")
    }

    /// Throw error string as an exception to the vm
    #[inline]
    fn throw_error(&self, mut msg: String) {
        msg.push('\0');
        // sq_throwerror copies the string
        unsafe { sq_throwerror(self.handle(), msg.as_ptr() as _); }
    }
}

/// Main SQVM trait
pub trait SQVm: SqVmErrorHandling
where Self: Sized {
    // VM functions

    /// Returns the execution state of a virtual machine
    #[inline]
    fn get_vm_state(&self) -> SqExecState {
        unsafe { sq_getvmstate(self.handle()) }.into()
    }

    /// Suspends the execution of the vm
    #[inline]
    fn suspend(&self) -> Result<()> {
        sq_try! { self, unsafe { sq_suspendvm(self.handle()) } }?;
        Ok(())
    }

    /// Wake up the execution a previously suspended virtual machine
    /// 
    /// if `resumed_ret = true` the function will pop a value 
    /// from the stack and use it as return value for the function
    /// that has previously suspended the virtual machine.
    /// 
    /// if `retval = true` the function will push the return value of the function
    /// that suspend the excution or the main function one.
    /// 
    /// if `raise_err = true`, if a runtime error occurs during the execution of
    /// the call, the vm will invoke the error handler.
    /// 
    /// if `throw_err = true`, the vm will thow an exception as soon as is resumed.
    /// the exception payload must be set beforehand invoking sq_throwerror().
    #[inline]
    fn wake_up(&self, resumed_ret: bool, retval: bool, raise_err: bool, throw_err: bool) -> Result<()> {
        sq_try! { self, unsafe {
            sq_wakeupvm(self.handle(), resumed_ret as _, retval as _, raise_err as _, throw_err as _)
        } }?;
        Ok(())
    }

    /// Creates a new friend vm of this one  
    /// and pushes it in its stack as "thread" object.
    #[inline]
    fn new_thread(&self, initial_stack_size: usize) -> FriendVm {
        let handle = unsafe {
             sq_newthread(self.handle(), initial_stack_size as _)
        };

        FriendVm(handle)
    }


    // Stack

    /// Pops `count` elements from the stack
    #[inline]
    fn pop(&self, count: SqInteger) {
        unsafe { sq_pop(self.handle(), count) }
    }

    // TODO: Check, how exactly this works
    /// Pushes copy(?) of an object at the `idx`
    #[inline]
    fn clone_idx(&self, idx: SqInteger) {
        unsafe { sq_push(self.handle(), idx) }
    }

    /// Pushes a weak reference or copy in case of value object at the `idx` in the stack
    #[inline]
    fn ref_idx(&self, idx: SqInteger) {
        unsafe { sq_weakref(self.handle(), idx) }
    }

    /// Removes an element from an arbitrary position in the stack
    #[inline]
    fn remove(&self, idx: SqInteger) {
        unsafe { sq_remove(self.handle(), idx) }
    }

    /// Returns the index of the top of the stack
    #[inline]
    fn stack_len(&self) -> SqInteger {
        unsafe { sq_gettop(self.handle()) } 
    }

    /// Resize the stack, if new `top` is bigger then the current top the function will push nulls.
    #[inline]
    fn set_stack_top(&self, top: usize) {
        unsafe { sq_settop(self.handle(), top as _) }
    }

    /// Ensure that the stack space left is at least of a specified `size`.
    /// If the stack is smaller it will automatically grow.
    #[inline]
    fn reserve_stack(&self, size: SqInteger) {
        unsafe { sq_reservestack(self.handle(), size) }
    } 

    /// Pushes the object at the position `idx` of the source vm stack in the destination vm stack
    #[inline]
    fn move_obj(&self, to: &SafeVm, idx: SqInteger) {
        unsafe { sq_move(to.handle, self.handle(), idx) }
    }

    /// Compares 2 objects from the stack.
    #[inline]
    fn stack_cmp(&self) -> Ordering {
        match unsafe { sq_cmp(self.handle()) } {
            1.. => Ordering::Greater,
            0 => Ordering::Equal,
            ..=-1 => Ordering::Less,
        }
    }


    // Object manipulation

    /// Get the type of the value at the position `idx` in the stack
    #[inline]
    fn get_type(&self, idx: SqInteger) -> SqType {
        unsafe { sq_gettype(self.handle(), idx) }.into()
    }

    /// Creates a new array and pushes it into the stack.
    #[inline]
    fn new_array(&self, size: usize) {
        unsafe { sq_newarray(self.handle(), size as _) }
    }

    /// Pops a value from the stack and pushes it in the back
    /// of the array at the position `idx` in the stack.
    #[inline]
    fn array_append(&self, idx: SqInteger) -> Result<()> {
        sq_try! { self, unsafe { sq_arrayappend(self.handle(), idx) } }
            .context(format!("Failed to insert value to array at index {idx}"))?;
        Ok(())
    } 

    
    /// Creates a new table and pushes it into the stack.
    #[inline]
    fn new_table(&self) {
        unsafe { sq_newtable(self.handle()) }
    }

    /// Pops a key and a value from the stack and performs a set operation
    /// on the table or class that is at position `idx` in the stack,
    /// if the slot does not exits it will be created.
    /// 
    /// if `is_static = true` creates a static member.
    /// This parameter is only used if the target object is a class
    #[inline]
    fn new_slot(&self, idx: SqInteger, is_static: bool) -> Result<()> {
        sq_try! { self, unsafe { sq_newslot(self.handle(), idx, is_static as _) } }
            .context(format!("Failed to create slot for table at idx {idx}"))?;
        Ok(())
    }

    /// Pops a key and a value from the stack and performs a set operation
    /// on the object at position `idx` in the stack.
    /// 
    /// this call will invoke the delegation system like a normal assignment,
    /// it only works on tables, arrays and userdata.
    #[inline]
    fn slot_set(&self, idx: SqInteger) -> Result<()> {
        sq_try! { self, unsafe { sq_set(self.handle(), idx) } }
            .context(format!("Failed to set slot value for table at idx {idx}"))?;
        Ok(())
    }

    /// Pops a key from the stack and performs a get operation on the object 
    /// at the position `idx` in the stack, and pushes the result in the stack.
    /// 
    /// This call will invoke the delegation system like a normal dereference. 
    /// It only works on tables, instances, arrays and classes.
    /// If the function fails nothing will be pushed in the stack.
    #[inline]
    fn slot_get(&self, idx: SqInteger) -> Result<()> {
        sq_try!{ self, unsafe { sq_get(self.handle(), idx) } }?;
        Ok(())
    }

    /// Pops a key from the stack and performs a get operation on the object 
    /// at the position `idx` in the stack, and pushes the result in the stack,
    /// without employing delegation or metamethods.
    ///  
    /// It only works on tables, instances, arrays and classes.
    /// If the function fails nothing will be pushed in the stack.
    #[inline]
    fn slot_get_raw(&self, idx: SqInteger) -> Result<()> {
        sq_try!{ self, unsafe { sq_rawget(self.handle(), idx) } }?;
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
        if unsafe { sq_next(self.handle(), idx) } >= 0 {
            Some(())
        } else {
            None
        }
    }

    /// Pushes the current root table in the stack
    #[inline]
    fn push_root_table(&self) {
        unsafe { sq_pushroottable(self.handle()) }
    } 

    /// Compiles a squirrel program.
    /// 
    /// if it succeeds, push the compiled script as function in the stack.
    /// 
    /// Args:
    /// - `src_name` - the symbolic name of the program (used only for debugging)
    /// - `raise_err` - if `true`, vm will call compiler error handler
    fn compile_string(&self, script: String, mut src_name: String, raise_err: bool) -> Result<()> {
        src_name.push('\0');

        sq_try! { self, nothrow unsafe { 
            sq_compilebuffer(
                self.handle(),
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
    fn get_instance_class(&self, idx: SqInteger) -> Result<()> {
        sq_try! { self,
            unsafe { sq_getclass(self.handle(), idx) }
        }?;
        Ok(())
    }

    /// Push info table of closure on stack index `idx` 
    fn get_closure_info(&self, idx: SqInteger) -> Result<()> {
        sq_try! { self,
            unsafe { sq_closure_getinfos(self.handle(), idx) } 
        }?;
        Ok(())
    }

    /// Pops an object from the stack (must be a table, instance or class) clones
    /// the closure at position `idx` in the stack and sets the popped object
    /// as environment of the cloned closure.
    ///
    /// Then pushes the new cloned closure on top of the stack
    ///
    /// The cloned closure holds the environment object as weak reference.
    fn bind_env(&self, idx: SqInteger) -> Result<()> {
        sq_try!{ self,
            unsafe { sq_bindenv(self.handle(), idx) }
        }?;
        Ok(())
    }
}

/// Iterator on squirrel array
pub struct SqArrayIter<'vm, VM, T> 
where VM: SqVmApi {
    vm: &'vm VM,
    max_depth: Option<u32>,
    _type: PhantomData<T>,
}

impl<VM, T> Drop for SqArrayIter<'_, VM, T>
where VM: SqVmApi {
    fn drop(&mut self) {
        // Pop the null iterator and the reference
        self.vm.pop(2);
    }
}

impl<VM, T> Iterator for SqArrayIter<'_, VM, T>
where 
    VM: SqVmApi + SqGet<T>
{
    type Item = Result<T>;

    fn next(&mut self) -> Option<Self::Item> {
        let next_depth = self.max_depth.map(|d| d - 1);
        self.vm.sq_iter_next(-3).map(|_| {
            let elem = self.vm.get_constrain(-1, next_depth);
            // Pop key-val
            if elem.is_ok() {
                self.vm.pop(2);
            }

            elem.context("Failed to get array value")
        })
    }
}

/// Iterator on squirrel table
pub struct SqTableIter<'vm, VM, K, V> 
where VM: SqVmApi {
    vm: &'vm VM,
    max_depth: Option<u32>,
    _type: PhantomData<(K, V)>,
}

impl<VM, K, V> Drop for SqTableIter<'_, VM, K, V>
where VM: SqVmApi {
    fn drop(&mut self) {
        // Pop the null iterator and the reference
        self.vm.pop(2);
    }
}

impl<VM, K, V> Iterator for SqTableIter<'_, VM, K, V>
where 
    VM: SqVmApi + SqGet<K> + SqGet<V>
{
    type Item = Result<(K, V)>;

    fn next(&mut self) -> Option<Self::Item> {
        let next_depth = self.max_depth.map(|d| d - 1);
        self.vm.sq_iter_next(-3).map(|_| {
            let val: V = match self.vm.get_constrain(-1, next_depth) {
                Ok(v) => v,
                Err(e) => return Err(e.context("Failed to get table value")),
            };

            let key: K = match self.vm.get_constrain(-2, next_depth) {
                Ok(k) => k,
                Err(e) => return Err(e.context("Failed to get table key")),
            };

            // Pop key-val
            self.vm.pop(2);
            Ok((key, val))
        })
    }
}

/// In-place squirrel iterators interface
pub trait SqVmIterators<'vm>: SqVmApi {

    /// Get rust iterator to squirrel array at index `idx`
    fn iter_array<T>(
        &'vm self,
        idx: SqInteger,
        max_depth: Option<u32>
    ) -> SqArrayIter<'vm, Self, T> {
        // Push a reference to an array to the stack top and a null iterator
        self.ref_idx(idx);
        self.push(SqNull).ok();

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
        self.push(SqNull).ok();

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
where VM: SqVmErrorHandling {
    pub name: String,
    pub handle: SqObjectRef<'vm, VM>,
}

/// Basic debug api methods
pub trait SqVmDebugBasic: SqVmApi + SqGet<DynSqVar> + SqPush<Box<SqFnClosure>>
where Self: Sized
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

        self.push(debug_hook_glue).expect("Failed to push debug hook closure");

        unsafe {
            self.set_debug_hook_raw();
        }
    }

    /// The member 'func_id' of the returned SqFunctionInfo structure is a
    /// unique identifier of the function; this can be useful to identify
    /// a specific piece of squirrel code in an application like for instance a profiler.
    /// this method will fail if the closure in the stack is a native C closure.
    /// 
    /// NOTE: Feels like legacy version of `get_stack_info`
    fn get_function_info(&self, level: SqInteger) -> Result<SqFunctionInfo> {
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
    fn get_stack_info(&self, level: SqUnsignedInteger) -> Result<SqStackInfo> {
        let mut stack_info = unsafe { std::mem::zeroed() };

        //  Error string isn`t pushed in this case
        sq_try! { self,
            nothrow unsafe { sq_stackinfos(self.handle(), level as _, addr_of_mut!(stack_info)) }
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
    ) -> Result<Option<SqLocalVar>> {
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
    ) -> Result<Option<SqLocalVarHandle<'_, Self>>> {
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

    /// Get size of vm calls stack
    #[inline]
    fn call_stack_len(&self) -> SqInteger {
        unsafe { sq_getcallstacksize(self.handle()) }
    }
}

/// Unsafe object manipulation
#[allow(clippy::missing_safety_doc)]
pub trait SqVmApi: SqVmErrorHandling
where Self: Sized
{
        /// Pushes a null value into the stack
        #[inline]
        unsafe fn push_null(&self) {
            sq_pushnull(self.handle())
        } 
    
        /// Copies and pushes a string into the stack
        #[inline]
        unsafe fn push_string(&self, s: *const u8, len: usize) {
            sq_pushstring(self.handle(), s as _, len as _)
        }
    
        /// Get a pointer to the string at the `idx` position in the stack.
        #[inline]
        unsafe fn get_string(&self, idx: SqInteger) -> Result<*const u8> {
            let mut ptr = std::ptr::null_mut();
            sq_try! { self,
                sq_getstring(self.handle(), idx, addr_of_mut!(ptr) as _) 
            }.context(format!("Failed to get string at idx {idx}"))?;
            Ok(ptr)
        }
    
        /// Pushes a integer into the stack
        #[inline]
        unsafe fn push_integer(&self, int: SqInteger) {
            sq_pushinteger(self.handle(), int)
        }
    
        // Get the value of the integer at the `idx` position in the stack.
        #[inline]
        unsafe fn get_integer(&self, idx: SqInteger) -> Result<SqInteger> {
            let mut out = 0;
            sq_try! { self,
                sq_getinteger(self.handle(), idx, addr_of_mut!(out)) 
            }.context(format!("Failed to get integer at idx {idx}"))?;
            Ok(out)
        }
    
        /// Pushes a bool into the stack
        #[inline]
        unsafe fn push_bool(&self, b: bool) {
            sq_pushbool(self.handle(), b as _)
        }
    
        /// Get the value of the bool at the `idx` position in the stack.
        #[inline]
        unsafe fn get_bool(&self, idx: SqInteger) -> Result<bool> {
            let mut out = 0;
            sq_try! { self,
                sq_getbool(self.handle(), idx, addr_of_mut!(out)) 
            }.context(format!("Failed to get bool at idx {idx}"))?;
            Ok(out != 0)
        }
    
        /// Pushes a float into the stack
        #[inline]
        unsafe fn push_float(&self, f: SqFloat) {
            sq_pushfloat(self.handle(), f)
        }
    
        /// Gets the value of the float at the idx position in the stack.
        #[inline]
        unsafe fn get_float(&self, idx: SqInteger) -> Result<SqFloat> {
            let mut out = 0.0;
            sq_try! { self,
                sq_getfloat(self.handle(), idx, addr_of_mut!(out)) 
            }.context(format!("Failed to get float at idx {idx}"))?; 
            Ok(out)
        }
    
        /// Pushes a userpointer into the stack
        #[inline]
        unsafe fn push_userpointer(&self, ptr: SQUserPointer) {
            sq_pushuserpointer(self.handle(), ptr)
        }
    
        /// Creates a new userdata and pushes it in the stack
        #[inline] 
        unsafe fn new_userdata(&self, size: SqUnsignedInteger) -> SQUserPointer {
            sq_newuserdata(self.handle(), size)
        }
    
        /// Gets a pointer to the value of the userdata at the `idx` position in the stack.
        /// 
        /// Returns (`ptr`, `type_tag`)
        /// * `ptr` - userpointer that will point to the userdata's payload
        /// * `type_tag` -  `SQUserPointer` that will store the userdata tag(see sq_settypetag).
        #[inline]
        unsafe fn get_userdata(&self, idx: SqInteger) -> Result<(SQUserPointer, SQUserPointer)> {
            let mut ptr = std::ptr::null_mut();
            let mut typetag = std::ptr::null_mut();
            sq_try! { self,
                sq_getuserdata(self.handle(), idx, addr_of_mut!(ptr), addr_of_mut!(typetag))
            }.context(format!("Failed to get userdata at idx {idx}"))?;
            Ok((ptr, typetag))
        }
    
        /// Get the value of the userpointer at the `idx` position in the stack.
        #[inline]
        unsafe fn get_userpointer(&self, idx: SqInteger) -> Result<SQUserPointer> {
            let mut ptr = std::ptr::null_mut();
            sq_try! { self,
                sq_getuserpointer(self.handle(), idx, addr_of_mut!(ptr))
            }.context(format!("Failed to get userpointer at idx {idx}"))?;
            Ok(ptr)
        }
    
        /// Returns the size of a value at the idx position in the stack
        /// Works only for arrays, tables, userdata, and strings
        #[inline]
        unsafe fn get_size(&self, idx: SqInteger) -> Result<SqInteger> {
            sq_try! { self,
                sq_getsize(self.handle(), idx)
            }.context(format!("Failed to get size of value at idx {idx}"))
        }
    
        /// Pops a closure from the stack an sets it as debug hook
        /// 
        /// In order to receive a 'per line' callback, is necessary 
        /// to compile the scripts with theline informations. 
        /// Without line informations activated, only the 'call/return' callbacks will be invoked.
        #[inline]
        unsafe fn set_debug_hook_raw(&self) {
            sq_setdebughook(self.handle());
        }
    
        /// Create a new native closure, pops `free_vars` values and set those
        /// as free variables of the new closure, and push the new closure in the stack
        #[inline]
        unsafe fn new_closure(&self, f: SqFunction, free_vars: SqUnsignedInteger) {
            sq_newclosure(self.handle(), Some(f), free_vars)
        }

        /// Sets a `hook` that will be called before release of __userdata__ at position `idx`
        #[inline]
        unsafe fn set_release_hook(&self, idx: SqInteger, hook: SqReleaseHook) -> Result<()> {
            if self.get_type(idx) != SqType::UserData {
                bail!("Value at idx {idx} isn`t a userdata");
            }
            sq_setreleasehook(self.handle(), idx, Some(hook));
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
        unsafe fn call_closure(&self, params: SqInteger, retval: bool, raise_error: bool) -> Result<()> {
            sq_try! { self,
                sq_call(self.handle(), params, retval as _, raise_error as _)
            }?;
            Ok(())
        }

        /// Set the compiler error handler function.
        /// 
        /// The compiler error handler is shared between friend VMs.
        #[inline]
        unsafe fn set_compiler_error_handler(&self, handler: Option<SqCompilerErrorHandler>) {
            let handler: SQCOMPILERERROR = handler.map(|h| h as _);
            sq_setcompilererrorhandler(self.handle(), handler);
        }
}

/// Advanced rust-wrapped operations
pub trait SqVmAdvanced<'a>:
    SqVmApi + SQVm + SqPush<&'a str> + SqPush<SqFunction> + SqGet<SqUserData>
    + SqGet<DynSqVar>
{
    // TODO: Add typemask
    
    /// Bind rust native function to root table of SQVM
    fn register_function(&self, name: &'a str, func: SqFunction) {
        self.push_root_table();
        self.push(name).expect("Failed to push closure name");
        self.push(func).expect("Failed to push function");
        self.new_slot(-3, false).expect("Failed to create slot in root table");
        self.pop(1);
    }

    /// Bind rust native closure to root table of SQVM 
    fn register_closure(&self, name: &'a str, closure: Box<SqFnClosure>) {
        self.push_root_table();
        self.push(name).expect("failed to push closure name");
        self.push(closure).expect("Failed to push closure");
        self.new_slot(-3, false).expect("Failed to create slot in root table");
        self.pop(1);
    }

    /// Compile and arbitrary squirrel script
    fn compile_closure(&self, script: String, src_file: String) -> Result<()> {

        /// This handler will push SqCompilerError ptr to stack as userdata
        extern "C" fn error_handler(
            vm: HSQUIRRELVM,
            desc: *const i8,
            src: *const i8,
            line: SqInteger,
            column: SqInteger,
        ) {
            unsafe { 
                let err = Box::new(SqCompilerError {
                    line,
                    column,
                    description: cstr_to_string(desc),
                    source: cstr_to_string(src),
                });

                let ptr = Box::leak(err) as *mut SqCompilerError;
                UnsafeVm::from_handle(vm).push(ptr)
                    .expect("Failed to push compiler error");
            }
        }
        
        let compile_res = unsafe { 
            self.set_compiler_error_handler(Some(error_handler));
            let res = self.compile_string(script, src_file, true);
            self.set_compiler_error_handler(None);
            res
        };

        if compile_res.is_err() {
            let err_box: SqUserPointer<SqCompilerError> = self.get(-1)
                .context("Failed to get error userdata")?;

            let err: Box<SqCompilerError> = unsafe {
                Box::from_raw(err_box)
            };

            // Pop error
            self.pop(1);

            bail!(err);
        };
        Ok(())
    }

    /// Call closure with specified argument count and return result.
    ///
    /// `depth` is depth of eager return value containers expansion.
    ///
    /// Returns [SqNull] if closure does not return anything.
    fn closure_call(&self, argc: SqInteger, depth: Option<SqUnsignedInteger>) -> Result<DynSqVar> {
        unsafe { self.call_closure(argc, true, false) }?;
        let ret = self.get_constrain(-1, depth)?;

        // Pop retval
        self.pop(1);
        Ok(ret)
    }
}

/// Handling raw objects
pub trait SqVmObjectHandling: SqVmErrorHandling {
    /// Adds a reference to an object handler.
    fn inc_ref(&self, obj: &mut SQObject) {
        unsafe { sq_addref(self.handle(), addr_of_mut!(*obj)) }
    }

    /// Remove a reference from an object handler.
    ///
    /// Returns `true` if object was deleted and if so, resets object handler to null.
    fn dec_ref(&self, obj: &mut SQObject) -> bool {
        (unsafe { sq_release(self.handle(), addr_of_mut!(*obj)) }) != 0
    }

    /// Initialize object handler.
    fn obj_init() -> SQObject {
        let mut obj = unsafe { std::mem::zeroed() };
        unsafe { sq_resetobject(addr_of_mut!(obj)) };
        obj
    }

    /// Gets an object (or it's pointer) from the stack and stores it in a object handler.
    fn get_stack_obj(&self, idx: SqInteger) -> Result<SQObject> {
        let mut obj = Self::obj_init();
        sq_try! { self,
            unsafe { sq_getstackobj(self.handle(), idx, addr_of_mut!(obj)) }
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
        unsafe { sq_pushobject(self.handle(), stub_wtf) }
    }
}


impl SqVmErrorHandling for SafeVm {}
impl SqVmErrorHandling for UnsafeVm {}
impl SqVmErrorHandling for FriendVm {}

impl<T: SqVmErrorHandling> SQVm for T {}
impl<T: SqVmErrorHandling> SqVmApi for T {}
impl<T: SqGet<DynSqVar> + SqVmApi> SqVmDebugBasic for T {}
impl<T: SqVmApi> SqVmIterators<'_> for T {}
impl<T: SqVmErrorHandling> SqVmObjectHandling for T {}

impl<'a, T> SqVmAdvanced<'a> for T
where 
    T: SqVmApi + SQVm + SqPush<&'a str> + SqPush<SqFunction>
{}

/// Trait for pushing values to vm stack
pub trait SqPush<T> {
    /// Push a value to the vm stack
    fn push(&self, val: T) -> Result<()>;
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
    fn get_constrain(&self, idx: SqInteger, max_depth: Option<u32>) -> Result<T>;
     
    /// Get value from the vm stack at position `idx`
    /// 
    /// if `idx` < 0, count from the top, else from the stack bottom
    /// 
    /// Do not limit recursion
    fn get(&self, idx: SqInteger) -> Result<T> {
        self.get_constrain(idx, None)
    }
}

/// Trait for throwing errors to vm
pub trait SqThrow<T> {
    /// Throw as a SQ exception to the vm
    fn throw(&self, throwable: T);
}


impl<T: SqVmErrorHandling> SqThrow<anyhow::Error> for T {
    fn throw(&self, throwable: anyhow::Error) {
        let msg = throwable.chain().into_iter()
            .fold(String::new(), |mut msg, e| {
                msg += "\nerror: ";
                msg += &e.to_string();
                msg
            });
        self.throw_error(msg);
    }
}

impl<VM> SqPush<SqBoxedClosure> for VM
where VM: SqPush<SqUserData> + SqVmApi
{
    fn push(&self, val: SqBoxedClosure) -> Result<()> {
        // Indirection to make fat pointer usable through FFI
        let clos_box = Box::new(val); 

        #[allow(clippy::borrowed_box)]
        extern "C" fn glue(hvm: HSQUIRRELVM) -> SqInteger {
            let mut vm = unsafe { UnsafeVm::from_handle(hvm).into_friend() };

            let top = vm.stack_len();
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

        self.push(SqUserData::from(data)).expect("Failed to push closure box ptr");
        unsafe { self.set_release_hook(-1, release_hook) }.expect("Failed to set box release hook");

        unsafe {
            self.new_closure(glue, 1);
        }

        Ok(())
    }
}

impl<T: SqVmApi> SqPush<SqFunction> for T {
    #[inline]
    fn push(&self, val: SqFunction) -> Result<()> {
        unsafe { self.new_closure(val, 0) }
        Ok(())
    }
}

impl<T: SqVmApi> SqPush<SqInteger> for T {
    #[inline]
    fn push(&self, val: SqInteger) -> Result<()> {
        unsafe { self.push_integer(val) }
        Ok(())
    }
}

impl<T: SqVmApi> SqGet<SqInteger> for T {
    #[inline]
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> Result<SqInteger> {
        unsafe { self.get_integer(idx) }
    }
}

impl<T: SqVmApi> SqPush<String> for T {
    #[inline]
    fn push(&self, val: String) -> Result<()> {
        unsafe { self.push_string(val.as_ptr(), val.len() as _) }
        Ok(())
    }
}

impl<T: SqVmApi> SqPush<&str> for T {
    #[inline]
    fn push(&self, val: &str) -> Result<()> {
        unsafe { self.push_string(val.as_ptr(), val.len() as _) }
        Ok(())
    }
}

// TODO: Use get_size() to make this more safe
impl<T: SqVmApi> SqGet<String> for T {
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> Result<String> {
        unsafe {
            let ptr = self.get_string(idx)?;
            Ok(cstr_to_string(ptr as _))
        }
    }
}

/// Just for abstraction...
impl<T: SqVmApi> SqPush<SqNull> for T {
    #[inline]
    fn push(&self, _: SqNull) -> Result<()> {
        unsafe { self.push_null(); }
        Ok(())
    }
}

/// For type safety
impl<T: SqVmApi> SqGet<SqNull> for T {
    #[inline]
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> Result<SqNull> {
        match self.get_type(idx) {
            SqType::Null => Ok(SqNull),
            other => 
                bail!("Failed to get null at idx {idx}: object type is {other:?}"),
        }
    }
}

impl<VM, T> SqPush<Vec<T>> for VM
where 
    VM: SqPush<T> + SqPush<SqInteger> + SqVmApi
{
    fn push(&self, val: Vec<T>) -> Result<()> {
        self.new_array(val.len());

        for (index, elem) in val.into_iter().enumerate() {
            self.push(index as SqInteger).context("Failed to push index to stack")?;
            self.push(elem).context("Failed to push element to stack")?;
            self.slot_set(-3)
                .context(format!("Failed to append element at index {index}"))?;
        }
        Ok(())
    }
}

impl<VM, T> SqGet<Vec<T>> for VM
where 
    VM: SqGet<T> + SqVmApi
{
    fn get_constrain(&self, idx: SqInteger, max_depth: Option<u32>) -> Result<Vec<T>> {
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
    VM: SqPush<K> + SqPush<V> + SqVmApi, 
{
    fn push(&self, val: IndexMap<K, V>) -> Result<()> {
        self.new_table();

        for (key, val) in val.into_iter() {
            self.push(key).context("Failed to push key to the stack")?;
            self.push(val).context("Failed to push value to the stack")?;

            self.new_slot(-3, false)
                .context("Failed to create new table slot")?;
        }
        Ok(())
    }
}

impl<VM, K, V> SqGet<IndexMap<K, V>> for VM
where 
    VM: SqGet<K> + SqGet<V> + SqVmApi,
    K: PartialEq + Eq + Hash,
{
    fn get_constrain(&self, idx: SqInteger, max_depth: Option<u32>) -> Result<IndexMap<K, V>> {
        if matches!(max_depth, Some(depth) if depth == 0) {
            return Ok(IndexMap::new());
        }

        let mut out = IndexMap::new();

        for pair in self.iter_table(idx, max_depth) {
            match pair {
                Ok((k, v)) => {
                    out.insert(k, v);
                },
                Err(e) => bail!(e),
            } 
        }

        Ok(out)
    }
}

impl<T: SqVmApi> SqPush<SqFloat> for T {
    #[inline]
    fn push(&self, val: SqFloat) -> Result<()> {
        unsafe { self.push_float(val) }
        Ok(())
    }
}

impl<T: SqVmApi> SqGet<SqFloat> for T {
    #[inline]
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> Result<SqFloat> {
        unsafe { self.get_float(idx) }
    }
}

impl<T: SqVmApi> SqPush<bool> for T {
    #[inline]
    fn push(&self, val: bool) -> Result<()> {
        unsafe { self.push_bool(val) }
        Ok(())
    }
}

impl<T: SqVmApi> SqGet<bool> for T {
    #[inline]
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> Result<bool> {
        unsafe { self.get_bool(idx) }
    }
}


impl<T: SqVmApi> SqPush<SqUserData> for T {
    #[inline]
    fn push(&self, val: SqUserData) -> Result<()> {
        let val = val.unwrap();
        unsafe { 
            let ptr = self.new_userdata(val.len() as _);
            std::ptr::copy(val.as_ptr() as _, ptr, val.len());
        }
        Ok(())
    }
}

impl<T: SqVmApi> SqGet<SqUserData> for T {
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> Result<SqUserData> {
        match unsafe { self.get_userdata(idx) } {
            Ok((user_data, _)) => {
                let out = unsafe {
                    let size = self.get_size(idx)? as usize;
                    let mut out = Vec::with_capacity(size);
                    std::ptr::copy(user_data, out.as_ptr() as _, size);
                    out.set_len(size);
                    SqUserData(out)
                };
                Ok(out)
            }
            Err(e) => Err(e) 
        }
    }
}


impl<VM, T> SqPush<Option<T>> for VM 
where 
    VM: SqPush<T> + SqVmApi
{
    fn push(&self, val: Option<T>) -> Result<()> {
        match val {
            Some(v) => self.push(v),
            None => SqPush::<SqNull>::push(self, SqNull),
        }
    }
}

impl<VM, T> SqGet<Option<T>> for VM 
where
    VM: SqGet<T> + SqVmApi
{
    fn get_constrain(&self, idx: SqInteger, max_depth: Option<u32>) -> Result<Option<T>> {
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
    VM: SqVmApi + SqGet<SqTable> + SqPush<DynSqVar>
{
    fn get_constrain(&self, idx: SqInteger, max_depth: Option<u32>) -> Result<SqInstance> {
        if matches!(max_depth, Some(depth) if depth == 0) {
            return Ok(SqInstance { this: IndexMap::new() });
        }

        // Get instance class table with keys and default values
        self.get_instance_class(idx)?;
        let proto: DynSqVar = self.get_constrain(-1, Some(1))?;

        let DynSqVar::Class(mut proto) = proto else { bail!("max depth") };
        self.pop(1);

        let next_depth = max_depth.map(|d| d - 1);
        for (key, val) in &mut proto {
            // Push class field/method key and get instance value
            self.push(key.clone())?;
            self.slot_get_raw(idx - idx.is_negative() as i32)?;

            *val = self.get_constrain(-1, next_depth)?;

            // Clear stack
            self.pop(1);
        }

        Ok(SqInstance { this: proto })
    }
}

// TODO: Get outer values 
#[derive(Clone, Debug)]
pub struct SqClosureInfo {
    name: Option<String>,
    args: Vec<String>,
    src: Option<String>,
}

impl<VM> SqGet<SqClosureInfo> for VM
where 
    VM: SqVmApi + SqGet<SqTable> 
{
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> Result<SqClosureInfo> {
        let mut info = SqClosureInfo { name: None, args: vec![], src: None };

        self.get_closure_info(idx)?;

        // Parameters are 1d array of strings
        for pair in self.iter_table(-1, Some(2)) {
            let (key, val): (String, DynSqVar) = match pair {
                Ok(kv) => kv,
                Err(e) => bail!(e),
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
    VM: SqVmApi + SqGet<SqTable>  
{
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> Result<SqNativeClosureInfo> {
        let mut info = SqNativeClosureInfo { name: None, arg_types: vec![] };

        self.get_closure_info(idx)?;

        // Argument types are 1d array of integers
        for pair in self.iter_table(-1, Some(2)) {
            let (key, val): (String, DynSqVar) = match pair {
                Ok(kv) => kv,
                Err(e) => bail!(e),
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
where VM: SqVmApi
{
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> Result<SqUserPointer<T>> {
        unsafe { self.get_userpointer(idx).map(|p| p as *mut T) }
    }
}

impl<VM, T> SqPush<SqUserPointer<T>> for VM 
where VM: SqVmApi 
{
    fn push(&self, val: SqUserPointer<T>) -> Result<()> {
        unsafe { self.push_userpointer(val as _) };
        Ok(())
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
            Self::NativeClosure(_) => SqType::Closure,
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
    VM: SqVmApi + SqGet<SqNull> // TODO: Why this is working?
    
{
    fn push(&self, val: DynSqVar) -> Result<()> {
        match val {
            DynSqVar::Null => self.push(SqNull),
            DynSqVar::Integer(i) => self.push(i),
            DynSqVar::String(s) => self.push(s),
            DynSqVar::Array(v) => self.push(v),
            DynSqVar::Float(f) => self.push(f),
            DynSqVar::Bool(b) => self.push(b),
            DynSqVar::Table(t) => self.push(t),
            DynSqVar::UserData(u) => self.push(u),
            _ => unimplemented!(),
        }
    }
}



impl<VM> SqGet<DynSqVar> for VM
where 
    VM: SqVmApi + SqGet<SqNull> 
{
    fn get_constrain(&self, idx: SqInteger, max_depth: Option<u32>) -> Result<DynSqVar> {
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

/// Type that is similar to rust's `()` and can be used for same purposes
pub struct SqUnit;

impl<VM> SqGet<SqUnit> for VM 
where VM: SqVmHandle {
    fn get_constrain(&self, _: SqInteger, _: Option<u32>) -> Result<SqUnit> {
        Ok(SqUnit)
    }
}

impl <VM> SqPush<SqUnit> for VM
where VM: SqVmHandle {
    fn push(&self, _: SqUnit) -> Result<()> {
        Ok(())
    }
}


/// Strong reference to squirrel vm object with RAII
pub struct SqObjectRef<'vm, VM>
where VM: SqVmObjectHandling + 'vm
{
    obj: SQObject,
    vm: &'vm VM
}

impl<'vm, VM> Drop for SqObjectRef<'vm, VM>
where VM: SqVmObjectHandling {
    fn drop(&mut self) {
        self.vm.dec_ref(&mut self.obj);
    }
}

impl<'vm, VM> SqObjectRef<'vm, VM>
where VM: SqVmObjectHandling {
    /// Get type of referenced object
    pub fn get_type(&self) -> SqType {
        self.obj._type.into()
    }

    /// Get a reference to object on the stack
    pub fn get(vm: &'vm VM, idx: SqInteger) -> Result<Self> {
        let mut obj = vm.get_stack_obj(idx)?;
        vm.inc_ref(&mut obj);
        Ok(SqObjectRef {
            obj,
            vm,
        })
    }
}

impl<VM> SqPush<SqObjectRef<'_, VM>> for VM
where VM: SqVmErrorHandling {
    fn push(&self, val: SqObjectRef<'_, VM>) -> Result<()> {
        self.push_stack_obj(&val.obj);
        Ok(())
    }
}