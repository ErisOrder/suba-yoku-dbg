use std::cmp::Ordering;
use std::marker::PhantomData;
use std::ptr::{addr_of_mut, addr_of};
use delegate::delegate;
use sq_macro::sq_closure;

use crate::{error::*, sq_validate};
use crate::util::cstr_to_string;

use super::api::{
    self, VmRawApi, SQ_ERROR, SqCompilerErrorHandler, 
    SqFunction, SqReleaseHook, SQObject, sq_resetobject
};
use super::get::{SqGet, SqGetResult};
use super::push::SqPush;
use super::iter::{SqArrayIter, SqTableIter};
use super::obj::SqObjectRef;
use super::types::*;


/// Strongly-typed vm errors
macro_rules! sq_try {
    ($vm:expr, $e:expr) => {
        {
            let out = $e;
            if out == SQ_ERROR as _ {
                let err = unsafe { $vm.last_error_cstr().map(|c| c.to_str()) }; 
                // TODO: Resolve issue with errors that were left not by this macro...
                $vm.api().reset_error();
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
            if out == SQ_ERROR as _ {
                Err(SqVmError::Other(None))
            } else {
                Ok(out)
            }
        }
    }
}

pub type SqVoidUserPointer = SqUserPointer<libc::c_void>;

/// Vm safety
pub mod safety {
    use super::api::VmRawApi;
        
    /// Vm drop specialization helper trait
    pub trait VmDrop where Self: Sized {
        fn drop_vm(vm: &mut super::Vm<Self>);
    }

    /// Automatically dropped
    pub struct Safe;

    /// Manually dropped
    pub struct Unsafe;

    /// Cannot be dropped (drops on main vm drop)
    pub struct Friend;

    impl VmDrop for Safe {
        fn drop_vm(vm: &mut super::Vm<Self>) {
            unsafe { vm.api().close() }
        }
    }

    impl VmDrop for Unsafe {
        fn drop_vm(_: &mut super::Vm<Self>) {}
    }
    
    impl VmDrop for Friend {
        fn drop_vm(_: &mut super::Vm<Self>) {}
    }
}

/// Event that VM debug hook may receive
#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum DebugEvent {
    /// Linenumber
    Line(isize),
    /// Function name, linenumber
    FnCall(String, Option<isize>),
    /// Function name, linenumber
    FnRet(String, Option<isize>),
}

/// DebugEvent bundled with source path
#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub struct DebugEventWithSrc {
    pub event: DebugEvent,
    pub src: Option<String>
}

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
    pub line: Option<isize>, 
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

/// SQVM  local variable
#[derive(Clone, Debug)]
pub struct SqLocalVar {
    pub name: String,
    pub val: DynSqVar,
}

/// SQVM local variable handle
pub struct SqLocalVarHandle<'vm, S> where S: safety::VmDrop {
    pub name: String,
    pub handle: SqObjectRef<'vm, S>,
}

/// Struct for accessing raw api methods of the vm
pub struct VmApi(api::HSQUIRRELVM);

impl api::VmRawApi for VmApi {
    fn handle(&self) -> squirrel2_kaleido_rs::HSQUIRRELVM {
        self.0
    }
}

impl<S> Drop for Vm<S> where S: safety::VmDrop {
    fn drop(&mut self) {
        S::drop_vm(self)
    }
} 

pub struct Vm<S> where S: safety::VmDrop  {
    api: VmApi,
    _safety: S
}

// Due to pointer in api
unsafe impl<S> Send for Vm<S> where S: safety::VmDrop {}

impl<S> Vm<S> where S: safety::VmDrop {
    /// Get underlying api
    pub fn api(&self) -> &VmApi {
        &self.api
    }

    /// Get last VM error.
    /// Return `None` if failed to decode string into utf-8. 
    /// # Panics
    /// Panics if failed to get error string.
    #[inline]
    pub fn last_error(&self) -> Option<String> {
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
        self.api().getlasterror();

        match self.get_type(-1) {
            SqType::Null => None,
            SqType::String => {
                let mut ptr = std::ptr::null();
                if self.api().getstring(-1, addr_of_mut!(ptr)) == SQ_ERROR as isize {
                    panic!("Failed to get last error")
                }
                Some(std::ffi::CStr::from_ptr(ptr))        
            }
            other => panic!("Unknown error type {other:?}"),
        }
    }

    /// Throw error string as an exception to the vm
    #[inline]
    pub fn throw_error(&self, mut msg: String) {
        msg.push('\0');
        // sq_throwerror copies the string
        unsafe { self.api().throwerror(msg.as_ptr() as _); }
    }

    /// Creates a new friend vm of this one
    /// and pushes it in its stack as "thread" object.
    #[inline]
    pub fn new_thread(&self, initial_stack_size: usize) -> Vm<safety::Friend> {
        unsafe {
            let handle = self.api().newthread(initial_stack_size as _);
            Vm::from_handle(handle).into_friend()
        }
    }

    /// Pushes the object at the position `idx` of the source vm stack in the destination vm stack
    #[inline]
    pub fn move_obj(&self, to: &Vm<safety::Safe>, idx: isize) {
        unsafe { self.api().move_object(to.api().handle(), idx) }
    }

    /// Compares 2 objects from the stack.
    #[inline]
    pub fn stack_cmp(&self) -> Ordering {
        match self.api().stack_compare() {
            1.. => Ordering::Greater,
            0 => Ordering::Equal,
            ..=-1 => Ordering::Less,
            _ => unreachable!()
        }
    }

    /// Creates a new array and pushes it into the stack.
    #[inline]
    pub fn new_array(&self, size: usize) {
        self.api().newarray(size as _)
    }

    /// Pops a value from the stack and pushes it in the back
    /// of the array at the position `idx` in the stack.
    #[inline]
    pub fn array_append(&self, idx: isize) -> SqVmResult<()> {
        sq_try! { self, unsafe { self.api().arrayappend(idx) } }?;
        Ok(())
    }
    
    /// Pops a key and a value from the stack and performs a set operation
    /// on the table or class that is at position `idx` in the stack,
    /// if the slot does not exits it will be created.
    /// 
    /// if `is_static = true` creates a static member.
    /// This parameter is only used if the target object is a class
    #[inline]
    pub fn new_slot(&self, idx: isize, is_static: bool) -> SqVmResult<()> {
        sq_try! { self, unsafe { self.api().newslot(idx, is_static as _) } }?;
        Ok(())
    }

    /// Pops a key and a value from the stack and performs a set operation
    /// on the object at position `idx` in the stack.
    /// 
    /// this call will invoke the delegation system like a normal assignment,
    /// it only works on tables, arrays and userdata.
    #[inline]
    pub fn slot_set(&self, idx: isize) -> SqVmResult<()> {
        sq_try! { self, self.api().set_to_slot(idx) }?;
        Ok(())
    }

    /// Pops a key from the stack and performs a get operation on the object 
    /// at the position `idx` in the stack, and pushes the result in the stack.
    /// 
    /// This call will invoke the delegation system like a normal dereference. 
    /// It only works on tables, instances, arrays and classes.
    /// If the function fails nothing will be pushed in the stack.
    #[inline]
    pub fn slot_get(&self, idx: isize) -> SqVmResult<()> {
        sq_try!{ self, self.api().get_from_slot(idx) }?;
        Ok(())
    }

    /// Pops a key from the stack and performs a get operation on the object 
    /// at the position `idx` in the stack, and pushes the result in the stack,
    /// without employing delegation or metamethods.
    ///  
    /// It only works on tables, instances, arrays and classes.
    /// If the function fails nothing will be pushed in the stack.
    #[inline]
    pub fn slot_get_raw(&self, idx: isize) -> SqVmResult<()> {
        sq_try!{ self, unsafe { self.api().rawget(idx) } }?;
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
    pub fn sq_iter_next(&self, idx: isize) -> Option<()> {
        if unsafe { self.api().next(idx) } >= 0 {
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
    pub fn compile_string(&self, script: String, mut src_name: String, raise_err: bool) -> SqVmResult<()> {
        src_name.push('\0');

        sq_try! { self, nothrow unsafe { 
            self.api().compilebuffer(
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
    pub fn get_instance_class(&self, idx: isize) -> SqVmResult<()> {
        sq_try! { self, unsafe { self.api().getclass(idx) } }?;
        Ok(())
    }

    /// Push info table of closure on stack index `idx` 
    pub fn get_closure_info(&self, idx: isize) -> SqVmResult<()> {
        sq_try! { self, unsafe { self.api().closure_getinfos(idx) } }?;
        Ok(())
    }

    /// Pops an object from the stack (must be a table, instance or class) clones
    /// the closure at position `idx` in the stack and sets the popped object
    /// as environment of the cloned closure.
    ///
    /// Then pushes the new cloned closure on top of the stack
    ///
    /// The cloned closure holds the environment object as weak reference.
    pub fn bind_env(&self, idx: isize) -> SqVmResult<()> {
        sq_try!{ self, unsafe { self.api().bindenv(idx) } }?;
        Ok(())
    }

    /// Get a pointer to the string at the `idx` position in the stack.
    #[inline]
    pub fn get_string(&self, idx: isize) -> SqVmResult<*const u8> {
        let mut ptr = std::ptr::null_mut();
        sq_try! { self,
            unsafe { self.api().getstring(idx, addr_of_mut!(ptr) as _) }
        }?;
        Ok(ptr)
    }

    /// Create a new native closure, pops `free_vars` values and set those
    /// as free variables of the new closure, and push the new closure in the stack
    #[inline]
    pub fn new_closure(&self, f: SqFunction, free_vars: usize) {
        unsafe { self.api().newclosure(Some(f), free_vars) }
    }

    /// Get the value of the integer at the `idx` position in the stack.
    #[inline]
    pub fn get_integer(&self, idx: isize) -> SqVmResult<isize> {
        let mut out = 0;
        sq_try! { self, nothrow unsafe { self.api().getinteger(idx,  addr_of_mut!(out)) }}?;
        Ok(out)
    }

    /// Get the value of the bool at the `idx` position in the stack.
    #[inline]
    pub fn get_bool(&self, idx: isize) -> SqVmResult<bool> {
        let mut out = 0;
        sq_try! { self, nothrow unsafe { self.api().getbool(idx, addr_of_mut!(out)) }}?;
        Ok(out != 0)
    }

    /// Gets the value of the float at the idx position in the stack.
    #[inline]
    pub fn get_float(&self, idx: isize) -> SqVmResult<SqFloat> {
        let mut out = 0.0;
        sq_try! { self, nothrow unsafe { self.api().getfloat(idx, addr_of_mut!(out)) }}?;
        Ok(out)
    }

    /// Gets a pointer to the value of the userdata at the `idx` position in the stack.
    ///
    /// Returns (`ptr`, `type_tag`)
    /// * `ptr` - userpointer that will point to the userdata's payload
    /// * `type_tag` -  `SQUserPointer` that will store the userdata tag(see sq_settypetag).
    #[inline]
    pub fn get_userdata(&self, idx: isize) -> SqVmResult<(SqVoidUserPointer, SqVoidUserPointer)> {
        let mut ptr = std::ptr::null_mut();
        let mut typetag = std::ptr::null_mut();
        sq_try! { self, nothrow
            unsafe { self.api().getuserdata(idx, addr_of_mut!(ptr), addr_of_mut!(typetag)) }
        }?;
        Ok((ptr as _, typetag as _))
    }

    /// Get the value of the userpointer at the `idx` position in the stack.
    #[inline]
    pub fn get_userpointer(&self, idx: isize) -> SqVmResult<SqVoidUserPointer> {
        let mut ptr = std::ptr::null_mut();
        sq_try! { self, nothrow
            unsafe { self.api().getuserpointer(idx, addr_of_mut!(ptr)) }
        }?;
        Ok(ptr as _)
    }

    /// Returns the size of a value at the idx position in the stack
    /// Works only for arrays, tables, userdata, and strings
    #[inline]
    pub fn get_size(&self, idx: isize) -> SqVmResult<isize> {
        sq_try! { self,
            unsafe { self.api().getsize(idx) }
        }
    }

    /// Sets a `hook` that will be called before release of __userdata__ at position `idx`
    #[inline]
    pub fn set_release_hook(&self, idx: isize, hook: SqReleaseHook) -> SqVmResult<()> {
        sq_validate!(self.get_type(idx), SqType::UserData)?;
        unsafe { self.api().setreleasehook(idx, Some(hook)) };
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
    pub fn call_closure_api(&self, params: isize, retval: bool, raise_error: bool) -> SqVmResult<()> {
        sq_try! { self,
            unsafe { self.api().call(params, retval as _, raise_error as _) }
        }?;
        Ok(())
    }

    /// Set the compiler error handler function.
    ///
    /// The compiler error handler is shared between friend VMs.
    #[inline]
    pub fn set_compiler_error_handler(&self, handler: Option<SqCompilerErrorHandler>) {
        unsafe { self.api().setcompilererrorhandler(handler); }
    }

    /// Adds a reference to an object handler.
    #[inline]
    pub fn inc_ref(&self, obj: &mut SQObject) {
        unsafe { self.api().addref(addr_of_mut!(*obj)) }
    }

    /// Remove a reference from an object handler.
    ///
    /// Returns `true` if object was deleted and if so, resets object handler to null.
    #[inline]
    pub fn dec_ref(&self, obj: &mut SQObject) -> bool {
        (unsafe { self.api().release(addr_of_mut!(*obj)) }) != 0
    }

    /// Initialize object handler.
    #[inline]
    pub fn obj_init() -> SQObject {
        let mut obj = unsafe { std::mem::zeroed() };
        // TODO: Make functions not related to VM associated to VmRawApi
        unsafe { sq_resetobject(addr_of_mut!(obj)) };
        obj
    }

    /// Gets an object (or it's pointer) from the stack and stores it in a object handler.
    #[inline]
    pub fn get_stack_obj(&self, idx: isize) -> SqVmResult<SQObject> {
        let mut obj = Self::obj_init();
        sq_try! { self,
            unsafe { self.api().getstackobj(idx, addr_of_mut!(obj)) }
        }?;
        Ok(obj)
    }

    /// Push an object referenced by an object handler into the stack.
    #[inline]
    pub fn push_stack_obj(&self, obj: &SQObject) {
        // Looks like sq_pushobject actually reads not object but ptr to it,
        // Despite the function signature. WTF?
        // So this struct's _type field used to pass pointer.
        let mut stub_wtf: SQObject = unsafe { std::mem::zeroed() };
        stub_wtf._type = addr_of!(*obj) as _;
        unsafe { self.api().pushobject(stub_wtf) }
    }

    // TODO: Add some lock during iteration
    /// Get rust iterator to squirrel array at index `idx`
    pub fn iter_array<T>(
        &self,
        idx: isize,
        max_depth: Option<usize>
    ) -> SqArrayIter<'_, S, T> {
        // Push a reference to an array to the stack top and a null iterator
        self.ref_idx(idx);
        self.api().push_null();

        SqArrayIter { vm: self, max_depth, _type: PhantomData }
    }

    /// Get rust iterator to squirrel table at index `idx`
    pub fn iter_table<K, V>(
        &self,
        idx: isize,
        max_depth: Option<usize>
    ) -> SqTableIter<'_, S, K, V> {
        // Push a reference to an array to the stack top and a null iterator
        self.ref_idx(idx);
        self.api().push_null();

        SqTableIter { vm: self, max_depth, _type: PhantomData }
    }

    /// Set VM debug hook that will be called for each line and function call/return
    /// 
    /// In order to receive a `line` callback, is necessary 
    /// to compile the scripts with the line informations. 
    /// Without line informations activated, only the `call/return` callbacks will be invoked.
    pub fn set_debug_hook<F>(&mut self, mut hook: F)
    where
        F: FnMut(DebugEventWithSrc, &Vm<safety::Friend>) + Send + 'static
    {
        let debug_hook_glue = sq_closure!(
            #[(vm_var = "vm", outer_crate = "crate")]
            move |
            event_type: isize,
            src: Option<String>,
            line: isize,
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
            self.api().setdebughook();
        }
    }

    /// The member 'func_id' of the returned SqFunctionInfo structure is a
    /// unique identifier of the function; this can be useful to identify
    /// a specific piece of squirrel code in an application like for instance a profiler.
    /// this method will fail if the closure in the stack is a native C closure.
    /// 
    /// NOTE: Feels like legacy version of `get_stack_info`
    pub fn get_function_info(&self, level: isize) -> SqVmResult<SqFunctionInfo> {
        let mut func_info = unsafe { std::mem::zeroed() };

        sq_try! { self,
            unsafe { self.api().getfunctioninfo(level, addr_of_mut!(func_info)) }
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
    pub fn get_stack_info(&self, level: usize) -> SqVmResult<SqStackInfo> {
        let mut stack_info = unsafe { std::mem::zeroed() };

        sq_try! { self, nothrow
            unsafe { self.api().stackinfos(level as _, addr_of_mut!(stack_info)) }
        }?;

        let name = unsafe { cstr_to_string(stack_info.funcname) };
        let src = unsafe { cstr_to_string(stack_info.source) };

        Ok(SqStackInfo { 
            funcname: if name != "unknown" { Some(name) } else { None },
            src_file: if src != "NATIVE" { Some(src) } else { None },
            line: if stack_info.line > 0 { Some(stack_info.line as isize) } else { None }
        })
    }

    /// Returns the name and value of a local variable given
    /// stackframe and sequence in the stack.
    /// 
    /// Free variables are treated as local variables, and will be returned
    /// as they would be at the base of the stack, just before the real local variable
    /// 
    /// Returns `None` if local on specified `idx` and `level` doesn't exist 
    pub fn get_local(
            &self,
            level: usize,
            idx: usize,
            max_depth: Option<usize>,
            ) -> SqGetResult<Option<SqLocalVar>> {
        let ptr = unsafe { self.api().getlocal(level, idx) };
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
    pub fn get_local_handle(
            &self,
            level: usize,
            idx: usize
    ) -> SqGetResult<Option<SqLocalVarHandle<'_, S>>> {
        let ptr = unsafe { self.api().getlocal(level, idx) };
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
    pub fn register_function(&self, name: &str, func: SqFunction) {
        self.push_root_table();
        self.push(name);
        self.push(func);
        self.new_slot(-3, false).expect("Failed to create slot in root table");
        self.pop(1);
    }

    /// Bind rust native closure to root table of SQVM 
    pub fn register_closure(&self, name: &str, closure: Box<SqFnClosure>) {
        self.push_root_table();
        self.push(name);
        self.push(closure);
        self.new_slot(-3, false).expect("Failed to create slot in root table");
        self.pop(1);
    }

    /// Compile and arbitrary squirrel script
    pub fn compile_closure(&self, script: String, src_file: String) -> SqCompilerResult<()> {

        /// This handler will push SqCompilerError ptr to stack as userdata
        extern "C" fn error_handler(
            vm: api::HSQUIRRELVM,
            desc: *const i8,
            src: *const i8,
            line: isize,
            column: isize,
        ) {
            unsafe { 
                // FIXME:
                let err = Box::new(SqCompilerError::CompileError {
                    line: line as _,
                    column: column as _,
                    description: cstr_to_string(desc),
                    src_file: cstr_to_string(src),
                });

                let ptr = Box::leak(err) as *mut SqCompilerError;
                Vm::from_handle(vm).push(ptr);
            }
        }

        self.api().enable_debug_info(true);    
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
    pub fn closure_call(&self, argc: isize, depth: Option<usize>) -> SqGetResult<DynSqVar> {
        self.call_closure_api(argc, true, false)
            .map_err(|e| e.into_stack_error("failed to call closure"))?;
        let ret = self.get_constrain(-1, depth)?;

        // Pop retval
        self.pop(1);
        Ok(ret)
    }

    // TODO: Make more api methods delegated through this macro
    delegate! {
        to self.api() {
            /// Get the type of the value at the position `idx` in the stack
            #[into]
            #[call(get_obj_type)]
            pub fn get_type(&self, idx: isize) -> SqType;
            pub fn pop(&self, count: isize);
            pub fn ref_idx(&self, idx: isize);
            pub fn push_root_table(&self);
        }
    }    
}

impl Vm<safety::Safe> {
    /// Creates a new instance of a squirrel VM that consists in a new execution stack.
    pub fn open(initial_stack_size: isize) -> Self {
        let handle = VmApi::open(initial_stack_size);
        Self {
            api: VmApi(handle),
            _safety: safety::Safe
        }
    }
}

impl Vm<safety::Unsafe> {
    /// Close this vm and all friend vms
    pub fn close(self) {
        unsafe { self.api().close(); }
    }

    /// Create struct from raw pointer to vm instance
    /// 
    /// # Safety
    /// Unsafe VM does not closed at drop
    pub unsafe fn from_handle(handle: api::HSQUIRRELVM) -> Self {
        Self {
            api: VmApi(handle),
            _safety: safety::Unsafe,
        }
    }
    
    /// Transform into Safe VM.
    /// Safe Vm will be closed on drop
    pub unsafe fn into_safe(self) -> Vm<safety::Safe> {
        // Reconstruct vm from handle, without running destructor
        let handle = self.api().handle();        
        std::mem::forget(self);
        Vm {
            api: VmApi(handle),
            _safety: safety::Safe,
        }
    }
    
    /// Transform into Friend VM.
    /// # Safety
    /// Friend Vm __cannot__ be closed
    pub unsafe fn into_friend(self) -> Vm<safety::Friend> {
        // Reconstruct vm from handle, without running destructor
        let handle = self.api().handle();        
        std::mem::forget(self);
        Vm {
            api: VmApi(handle),
            _safety: safety::Friend,
        }
    }
}

