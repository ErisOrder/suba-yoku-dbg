use anyhow::{bail, Context};
use squirrel2_kaleido_rs::*;
use util_proc_macro::sqfn;
use std::{ptr::{addr_of_mut, addr_of}, cmp::Ordering, collections::HashMap, hash::Hash};
use anyhow::{
    Result,
    anyhow
};

macro_rules! sq_try {
    ($vm:expr, $e:expr) => {
        {
            let out = $e;
            if out == -1 {
                Err(anyhow!($vm.last_error()))
            } else { Ok(out) }
        }
    };
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

pub type SqDebugHook<'a> = dyn Fn(DebugEvent, Option<String>) + Send + 'a;

/// C SQFunction type 
pub type SQFn = unsafe extern "C" fn(HSQUIRRELVM) -> SQInteger;

/// Safe abstraction for SQFn
pub type SQFnClosure<'a> = dyn Fn(&mut FriendVm) -> SQInteger + Send + 'a; 

/// Event that VM debug hook may receive
#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum DebugEvent {
    /// Linenumber
    Line(SQInteger),
    /// Function name, linenumber
    FnCall(String, Option<SQInteger>),
    /// Function name, linenumber
    FnRet(String, Option<SQInteger>),
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

    /// Stub for unknown or unsupported types
    Unknown(SQInteger)
}

#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub struct SQNull;

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
            unk => SqType::Unknown(unk)
        }
    }
}

/// Allows to get vm handle from struct
pub trait SqVmHandle {
    fn handle(&mut self) -> HSQUIRRELVM;
}

/// Safe wrapper around SQVM handle.
/// Will be closed on drop 
pub struct SafeVm<'a> {
    handle: HSQUIRRELVM,
    debug_hook: Option<Box<Box<SqDebugHook<'a>>>>,
    native_closures: HashMap<String, Box<Box<SQFnClosure<'a>>>>
}

/// It`s not safe, but it`s needed
unsafe impl Send for SafeVm<'_> {}

impl SqVmHandle for SafeVm<'_> {
    fn handle(&mut self) -> HSQUIRRELVM {
        self.handle
    }
}

impl Drop for SafeVm<'_> {
    fn drop(&mut self) {
        unsafe { sq_close(self.handle) }
    }
}

impl<'a, 'b> SafeVm<'a>
where
    'a: 'b
{
    /// Creates a new instance of a squirrel VM that consists in a new execution stack.
    pub fn open(initial_stack_size: usize) -> Self {
        let handle = unsafe {
             sq_open(initial_stack_size as _)
        };
        Self { handle, debug_hook: None, native_closures: HashMap::new() }
    }

    
    /// Set VM debug hook that will be called for each line and function call/return
    /// 
    /// In order to receive a 'per line' callback, is necessary 
    /// to compile the scripts with the line informations. 
    /// Without line informations activated, only the 'call/return' callbacks will be invoked.
    pub fn set_debug_hook(&'b mut self, hook: Box<SqDebugHook<'a>>) {
        // Indirection to make fat pointer usable through FFI
        let hook_box = Box::new(hook); 

        #[allow(clippy::borrowed_box)]
        #[sqfn(sq_lib_path = "squirrel2_kaleido_rs", sq_wrap_path = "self")]
        fn DebugHook(
            event_type: SQInteger,
            src_file: Option<String>,
            line: Option<SQInteger>,
            funcname: Option<String>,
            closure_box: SqUserData, // "captured"
        ) {

            let event = match char::from_u32(event_type as u32).unwrap() {
                'l' => DebugEvent::Line(line.unwrap()),
                'c' => DebugEvent::FnCall(funcname.unwrap_or_else(|| "??".into()), line),
                'r' => DebugEvent::FnRet(funcname.unwrap_or_else(|| "??".into()), line),
                e => panic!("unknown debug event: {e}"),
            };

            let closure_box: usize = unsafe { std::ptr::read(closure_box.unwrap().as_ptr() as _) };

            // Must be safe as long as wrapper bound with VM is alive
            let hook: &Box<SqDebugHook> = unsafe {
                &*(closure_box as *mut _) 
            };

            hook(event, src_file);
        }

        let raw = Box::leak(hook_box) as *mut _;
        
        // Just to drop it if hook reassigned
        self.debug_hook = Some(unsafe { Box::from_raw(raw) });

        let data: Vec<_> = (raw as usize).to_ne_bytes().into();
        self.push(SqUserData::from(data)).expect("Failed to push hook closure box ptr");

        unsafe {
            self.new_closure(DebugHook::sq_fn, 1);
            self.set_debug_hook_raw();
        }
    }
    
    pub fn register_closure(&'b mut self, name: &str, closure: Box<SQFnClosure<'a>>) {
        let clos_box = Box::new(closure); 

        #[allow(clippy::borrowed_box)]
        unsafe extern "C" fn glue(hvm: HSQUIRRELVM) -> SQInteger {
            let mut vm = UnsafeVm::from_handle(hvm).into_friend();

            let top = vm.stack_len();
            let closure_box: SqUserData = vm.get(top).unwrap();

            let closure_box: usize = unsafe { std::ptr::read(closure_box.unwrap().as_ptr() as _) };

            // Must be safe as long as wrapper bound with VM is alive
            let closure: &Box<SQFnClosure> = unsafe {
                &*(closure_box as *mut _) 
            };

            closure(&mut vm)
        }

        let raw = Box::leak(clos_box) as *mut _;

        let boxed_box = unsafe { Box::from_raw(raw) };

        let old = self.native_closures.insert(name.to_string(), boxed_box);

        let data: Vec<_> = (raw as usize).to_ne_bytes().into();

        self.push_root_table();
        self.push(name).expect("failed to push closure name");
        self.push(SqUserData::from(data)).expect("Failed to push closure box ptr");
        
        unsafe {
            self.new_closure(glue, 1);
        }
            
        if old.is_some() {
            self.slot_set(-3).expect("Failed to set root table slot value");
        } else {
            self.new_slot(-3, false).expect("Failed to create slot in root table");
        }
        
        self.pop(1);
    }

}

/// Wrapper for manipulating foreign VMs
/// Will not be closed automatically
pub struct UnsafeVm(HSQUIRRELVM);

impl SqVmHandle for UnsafeVm {
    fn handle(&mut self) -> HSQUIRRELVM {
        self.0
    }
}

impl UnsafeVm {
    /// Create struct from raw pointer to vm instance
    /// 
    /// ## Safety
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
    pub fn into_safe(self) -> SafeVm<'static> {
        SafeVm { handle: self.0, debug_hook: None, native_closures: HashMap::new() }
    }

    /// Transform into Friend VM.
    /// Friend Vm __cannot__ be closed
    pub unsafe fn into_friend(self) -> FriendVm {
        FriendVm(self.0)
    }

}

/// Wrapper for friend SQVM.
/// Cannot be closed by user
pub struct FriendVm(HSQUIRRELVM);

impl SqVmHandle for FriendVm {
    fn handle(&mut self) -> HSQUIRRELVM {
        self.0
    }
}

pub enum ExecState {
 
}

// TODO: Somehow remove cyclic dependency...
/// Getting and throwing VM errors
pub trait SqVmErrorHandling: SqVmHandle + SqGet<String> {
    /// Get last VM error
    fn last_error(&mut self) -> String {
        unsafe { sq_getlasterror(self.handle()) }
        self.get(-1).expect("Failed to get last error")
    }

    /// Throw error string as an exception to the vm
    fn throw_error(&mut self, mut msg: String) {
        msg.push('\0');
        unsafe { sq_throwerror(self.handle(), msg.as_ptr() as _); }
    }
}

/// Main SQVM trait
pub trait SQVm: SqVmErrorHandling {
    // VM functions

    /// Returns the execution state of a virtual machine
    fn get_vm_state(&self) -> ExecState {
        todo!()
    }

    /// Suspends the execution of the vm
    fn suspend(&mut self) -> Result<()> {
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
    fn wake_up(&mut self, resumed_ret: bool, retval: bool, raise_err: bool, throw_err: bool) -> Result<()> {
        sq_try! { self, unsafe {
            sq_wakeupvm(self.handle(), resumed_ret as _, retval as _, raise_err as _, throw_err as _)
        } }?;
        Ok(())
    }

    /// Creates a new friend vm of this one  
    /// and pushes it in its stack as "thread" object.
    fn new_thread(&mut self, initial_stack_size: usize) -> FriendVm {
        let handle = unsafe {
             sq_newthread(self.handle(), initial_stack_size as _)
        };

        FriendVm(handle)
    }


    // Stack

    /// Pops `count` elements from the stack
    fn pop(&mut self, count: SQInteger) {
        unsafe { sq_pop(self.handle(), count) }
    }

    // TODO: Check, how exactly this works
    /// Pushes copy(?) of an object at the `idx`
    fn clone_idx(&mut self, idx: SQInteger) {
        unsafe { sq_push(self.handle(), idx) }
    }

    /// Pushes a weak reference or copy in case of value object at the `idx` in the stack
    fn ref_idx(&mut self, idx: SQInteger) {
        unsafe { sq_weakref(self.handle(), idx) }
    }

    /// Removes an element from an arbitrary position in the stack
    fn remove(&mut self, idx: SQInteger) {
        unsafe { sq_remove(self.handle(), idx) }
    }

    /// Returns the index of the top of the stack
    fn stack_len(&mut self) -> SQInteger {
        unsafe { sq_gettop(self.handle()) } 
    }

    /// Resize the stack, if new `top` is bigger then the current top the function will push nulls.
    /// 
    /// ## Safety
    /// VM execution may fail if stack resize is unexpected
    unsafe fn set_stack_top(&mut self, top: usize) {
        sq_settop(self.handle(), top as _)
    }

    /// Ensure that the stack space left is at least of a specified `size`.
    /// If the stack is smaller it will automatically grow.
    fn reserve_stack(&mut self, size: SQInteger) {
        unsafe { sq_reservestack(self.handle(), size) }
    } 

    /// Pushes the object at the position `idx` of the source vm stack in the destination vm stack
    fn move_obj(&mut self, to: &mut SafeVm, idx: SQInteger) {
        unsafe { sq_move(to.handle, self.handle(), idx) }
    }

    /// Compares 2 objects from the stack.
    fn stack_cmp(&mut self) -> Ordering {
        match unsafe { sq_cmp(self.handle()) } {
            1.. => Ordering::Greater,
            0 => Ordering::Equal,
            SQInteger::MIN..=-1 => Ordering::Less,
        }
    }


    // Object manipulation

    /// Get the type of the value at the position `idx` in the stack
    #[inline]
    fn get_type(&mut self, idx: SQInteger) -> SqType {
        unsafe { sq_gettype(self.handle(), idx) }.into()
    }

    /// Creates a new array and pushes it into the stack.
    fn new_array(&mut self, size: usize) {
        unsafe { sq_newarray(self.handle(), size as _) }
    }

    /// Pops a value from the stack and pushes it in the back
    /// of the array at the position `idx` in the stack.
    fn array_append(&mut self, idx: SQInteger) -> Result<()> {
        sq_try! { self, unsafe { sq_arrayappend(self.handle(), idx) } }
            .context(format!("Failed to insert value to array at index {idx}"))?;
        Ok(())
    } 

    
    /// Creates a new table and pushes it into the stack.
    fn new_table(&mut self) {
        unsafe { sq_newtable(self.handle()) }
    }

    /// Pops a key and a value from the stack and performs a set operation
    /// on the table or class that is at position `idx` in the stack,
    /// if the slot does not exits it will be created.
    /// 
    /// if `is_static = true` creates a static member.
    /// This parameter is only used if the target object is a class
    fn new_slot(&mut self, idx: SQInteger, is_static: bool) -> Result<()> {
        sq_try! { self, unsafe { sq_newslot(self.handle(), idx, is_static as _) } }
            .context(format!("Failed to create slot for table at idx {idx}"))?;
        Ok(())
    }

    /// Pops a key and a value from the stack and performs a set operation
    /// on the object at position `idx` in the stack.
    /// 
    /// this call will invoke the delegation system like a normal assignment,
    /// it only works on tables, arrays and userdata.
    fn slot_set(&mut self, idx: SQInteger) -> Result<()> {
        sq_try! { self, unsafe { sq_set(self.handle(), idx) } }
            .context(format!("Failed to set slot value for table at idx {idx}"))?;
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
    fn sq_iter_next(&mut self, idx: SQInteger) -> Result<()> {
        if unsafe { sq_next(self.handle(), idx) } >= 0 {
            Ok(())
        } else {
            bail!("Failed to iterate using iterator at index {idx}")
        }
    }

    /// Pushes the current root table in the stack
    fn push_root_table(&mut self) {
        unsafe { sq_pushroottable(self.handle()) }
    } 
    
}

/// Unsafe object manipulation
pub trait SqVmApi: SqVmErrorHandling {
        /// Pushes a null value into the stack
        #[inline]
        unsafe fn push_null(&mut self) {
            sq_pushnull(self.handle())
        } 
    
        /// Copies and pushes a string into the stack
        #[inline]
        unsafe fn push_string(&mut self, s: *const u8, len: usize) {
            sq_pushstring(self.handle(), s as _, len as _)
        }
    
        /// Get a pointer to the string at the `idx` position in the stack.
        unsafe fn get_string(&mut self, idx: SQInteger) -> Result<*const u8> {
            let mut ptr = std::ptr::null_mut();
            sq_try! { self,
                sq_getstring(self.handle(), idx, addr_of_mut!(ptr) as _) 
            }.context(format!("Failed to get string at idx {idx}"))?;
            Ok(ptr)
        }
    
        /// Pushes a integer into the stack
        #[inline]
        unsafe fn push_integer(&mut self, int: SQInteger) {
            sq_pushinteger(self.handle(), int)
        }
    
        // Get the value of the integer at the `idx` position in the stack.
        unsafe fn get_integer(&mut self, idx: SQInteger) -> Result<SQInteger> {
            let mut out = 0;
            sq_try! { self,
                sq_getinteger(self.handle(), idx, addr_of_mut!(out)) 
            }.context(format!("Failed to get integer at idx {idx}"))?;
            Ok(out)
        }
    
        /// Pushes a bool into the stack
        #[inline]
        unsafe fn push_bool(&mut self, b: bool) {
            sq_pushbool(self.handle(), b as _)
        }
    
        /// Get the value of the bool at the `idx` position in the stack.
        unsafe fn get_bool(&mut self, idx: SQInteger) -> Result<bool> {
            let mut out = 0;
            sq_try! { self,
                sq_getbool(self.handle(), idx, addr_of_mut!(out)) 
            }.context(format!("Failed to get bool at idx {idx}"))?;
            Ok(out != 0)
        }
    
        /// Pushes a float into the stack
        #[inline]
        unsafe fn push_float(&mut self, f: SQFloat) {
            sq_pushfloat(self.handle(), f)
        }
    
        /// Gets the value of the float at the idx position in the stack.
        unsafe fn get_float(&mut self, idx: SQInteger) -> Result<SQFloat> {
            let mut out = 0.0;
            sq_try! { self,
                sq_getfloat(self.handle(), idx, addr_of_mut!(out)) 
            }.context(format!("Failed to get float at idx {idx}"))?; 
            Ok(out)
        }
    
        /// Pushes a userpointer into the stack
        unsafe fn push_userpointer(&mut self, ptr: SQUserPointer) {
            sq_pushuserpointer(self.handle(), ptr)
        }
    
        /// Creates a new userdata and pushes it in the stack
        unsafe fn new_userdata(&mut self, size: SQUnsignedInteger) -> SQUserPointer {
            sq_newuserdata(self.handle(), size)
        }
    
        /// Gets a pointer to the value of the userdata at the `idx` position in the stack.
        /// 
        /// Returns (`ptr`, `type_tag`)
        /// * `ptr` - userpointer that will point to the userdata's payload
        /// * `type_tag` -  `SQUserPointer` that will store the userdata tag(see sq_settypetag).
        unsafe fn get_userdata(&mut self, idx: SQInteger) -> Result<(SQUserPointer, SQUserPointer)> {
            let mut ptr = std::ptr::null_mut();
            let mut typetag = std::ptr::null_mut();
            sq_try! { self,
                sq_getuserdata(self.handle(), idx, addr_of_mut!(ptr), addr_of_mut!(typetag))
            }.context(format!("Failed to get userdata at idx {idx}"))?;
            Ok((ptr, typetag))
        }
    
        /// Get the value of the userpointer at the `idx` position in the stack.
        unsafe fn get_userpointer(&mut self, idx: SQInteger) -> Result<SQUserPointer> {
            let mut ptr = std::ptr::null_mut();
            sq_try! { self,
                sq_getuserpointer(self.handle(), idx, addr_of_mut!(ptr))
            }.context(format!("Failed to get userpointer at idx {idx}"))?;
            Ok(ptr)
        }
    
        /// Returns the size of a value at the idx position in the stack
        /// Works only for arrays, tables, userdata, and strings
        unsafe fn get_size(&mut self, idx: SQInteger) -> Result<SQInteger> {
            sq_try! { self,
                sq_getsize(self.handle(), idx)
            }.context(format!("Failed to get size of value at idx {idx}"))
        }
    
        /// Pops a closure from the stack an sets it as debug hook
        /// 
        /// In order to receive a 'per line' callback, is necessary 
        /// to compile the scripts with theline informations. 
        /// Without line informations activated, only the 'call/return' callbacks will be invoked.
        unsafe fn set_debug_hook_raw(&mut self) {
            sq_setdebughook(self.handle());
        }
    
        /// Create a new native closure, pops `free_vars` values and set those
        /// as free variables of the new closure, and push the new closure in the stack
        unsafe fn new_closure(&mut self, f: SQFn, free_vars: SQUnsignedInteger) {
            sq_newclosure(self.handle(), Some(f), free_vars)
        }
}

impl SqVmErrorHandling for SafeVm<'_> {}
impl SqVmErrorHandling for UnsafeVm {}
impl SqVmErrorHandling for FriendVm {}

impl<T: SqVmErrorHandling> SQVm for T {}
impl<T: SqVmErrorHandling> SqVmApi for T {}

pub trait SqPush<T> {
    /// Push a value to the vm stack
    fn push(&mut self, val: T) -> Result<()>;
}

pub trait SqGet<T> {
    /// Get value from the vm stack at position `idx`
    /// 
    /// if `idx` < 0, count from the top, else from the stack bottom
    fn get(&mut self, idx: SQInteger) -> Result<T>;
}

pub trait SqThrow<T> {
    /// Throw as a SQ exception to the vm
    fn throw(&mut self, throwable: T);
}


impl<T: SqVmErrorHandling> SqThrow<anyhow::Error> for T {
    fn throw(&mut self, throwable: anyhow::Error) {
        let msg = throwable.chain().into_iter()
            .fold(String::new(), |mut msg, e| {
                msg += "\nerror: ";
                msg += &e.to_string();
                msg
            });
        self.throw_error(msg);
    }
}

impl<T: SqVmApi> SqPush<SQFn> for T {
    fn push(&mut self, val: SQFn) -> Result<()> {
        unsafe { self.new_closure(val, 0) }
        Ok(())
    }
}

impl<T: SqVmApi> SqPush<SQInteger> for T {
    fn push(&mut self, val: SQInteger) -> Result<()> {
        unsafe { self.push_integer(val) }
        Ok(())
    }
}

impl<T: SqVmApi> SqGet<SQInteger> for T {
    fn get(&mut self, idx: SQInteger) -> Result<SQInteger> {
        unsafe { self.get_integer(idx) }
    }
}

impl<T: SqVmApi> SqPush<String> for T {
    fn push(&mut self, val: String) -> Result<()> {
        unsafe { self.push_string(val.as_ptr(), val.len() as _) }
        Ok(())
    }
}

impl<T: SqVmApi> SqPush<&str> for T {
    fn push(&mut self, val: &str) -> Result<()> {
        unsafe { self.push_string(val.as_ptr(), val.len() as _) }
        Ok(())
    }
}

// TODO: Use get_size() to make this more safe
impl<T: SqVmApi> SqGet<String> for T {
    fn get(&mut self, idx: SQInteger) -> Result<String> {
        let ptr = unsafe { self.get_string(idx) }?;

        unsafe {
            let len = libc::strlen(ptr as _);
            let mut v = Vec::with_capacity(len);
            std::ptr::copy(ptr, v.as_mut_ptr(), len);
            v.set_len(len);
            Ok(String::from_utf8_unchecked(v)) 
        }
    }
}

/// Just for abstraction...
impl<T: SqVmApi> SqPush<SQNull> for T {
    fn push(&mut self, _: SQNull) -> Result<()> {
        unsafe { self.push_null(); }
        Ok(())
    }
}

/// For type safety
impl<T: SqVmApi> SqGet<SQNull> for T {
    fn get(&mut self, idx: SQInteger) -> Result<SQNull> {
        match self.get_type(idx) {
            SqType::Null => Ok(SQNull),
            SqType::Unknown(id) => 
                bail!("Failed to get null at idx {idx}: unknown type 0x{id:x}"),
            other => 
                bail!("Failed to get null at idx {idx}: object type is {other:?}"),
        }
    }
}

impl<VM, T> SqPush<Vec<T>> for VM
where 
    VM: SqPush<T> + SqVmApi
{
    fn push(&mut self, val: Vec<T>) -> Result<()> {
        self.new_array(val.len());

        for (index, elem) in val.into_iter().enumerate() {
            self.push(elem).context("Failed to push element to stack")?;
            self.array_append(-2)
                .context(format!("Failed to append element at index {index}"))?;
        }
        Ok(())
    }
}

impl<VM, T> SqGet<Vec<T>> for VM
where 
    VM: SqGet<T> + SqVmApi
{
    fn get(&mut self, idx: SQInteger) -> Result<Vec<T>> {
        // Push a reference to an array to the stack top and a null iterator
        self.ref_idx(idx);
        self.push(SQNull)?;

        let mut out = vec![];

        while self.sq_iter_next(-3).is_ok() {
            let elem: T = self.get(-1).context("Failed to get array value")?;
            out.push(elem);

            self.pop(2);
        }
        // Pop the null iterator and the reference
        self.pop(2);
        Ok(out)
    }
}

impl<VM, K, V> SqPush<HashMap<K, V>> for VM
where 
    VM: SqPush<K> + SqPush<V> + SqVmApi, 
{
    fn push(&mut self, val: HashMap<K, V>) -> Result<()> {
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

impl<VM, K, V> SqGet<HashMap<K, V>> for VM
where 
    VM: SqGet<K> + SqGet<V> + SqVmApi,
    K: PartialEq + Eq + Hash,
{
    fn get(&mut self, idx: SQInteger) -> Result<HashMap<K, V>> {
        // Push a reference to a table to the stack top and a null iterator
        self.ref_idx(idx);
        self.push(SQNull)?;

        let mut out = HashMap::new();

        while self.sq_iter_next(-3).is_ok() {
            let val: V = self.get(-1).context("Failed to get table value")?;
            let key: K = self.get(-2).context("Failed to get table key")?;

            out.insert(key, val);

            self.pop(2);
        }
        // Pop the null iterator and the reference
        self.pop(2);
        Ok(out)
    }
}

impl<T: SqVmApi> SqPush<SQFloat> for T {
    fn push(&mut self, val: SQFloat) -> Result<()> {
        unsafe { self.push_float(val) }
        Ok(())
    }
}

impl<T: SqVmApi> SqGet<SQFloat> for T {
    fn get(&mut self, idx: SQInteger) -> Result<SQFloat> {
        unsafe { self.get_float(idx) }
    }
}

impl<T: SqVmApi> SqPush<bool> for T {
    fn push(&mut self, val: bool) -> Result<()> {
        unsafe { self.push_bool(val) }
        Ok(())
    }
}

impl<T: SqVmApi> SqGet<bool> for T {
    fn get(&mut self, idx: SQInteger) -> Result<bool> {
        unsafe { self.get_bool(idx) }
    }
}


impl<T: SqVmApi> SqPush<SqUserData> for T {
    fn push(&mut self, val: SqUserData) -> Result<()> {
        let val = val.unwrap();
        unsafe { 
            let ptr = self.new_userdata(val.len() as _);
            std::ptr::copy(val.as_ptr() as _, ptr, val.len());
        }
        Ok(())
    }
}

impl<T: SqVmApi> SqGet<SqUserData> for T {
    fn get(&mut self, idx: SQInteger) -> Result<SqUserData> {
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
    fn push(&mut self, val: Option<T>) -> Result<()> {
        match val {
            Some(v) => self.push(v),
            None => SqPush::<SQNull>::push(self, SQNull),
        }
    }
}

impl<VM, T> SqGet<Option<T>> for VM 
where
    VM: SqGet<T> + SqVmApi
{
    fn get(&mut self, idx: SQInteger) -> Result<Option<T>> {
        if SqGet::<SQNull>::get(self, idx).is_ok() {
            Ok(None)
        } else {
            Ok(Some(SqGet::<T>::get(self, idx)?))
        }
    }
}

#[derive(Clone, Debug)]
pub enum DynSqVar {
    Null,
    Integer(SQInteger),
    Float(SQFloat),
    Bool(bool),
    String(String),
    Table(HashMap<DynSqVar, DynSqVar>),
    Array(Vec<DynSqVar>),
    UserData(SqUserData),
    //UserPointer(???),
    //Class(SQClass),
    //Weakref(???)
} 

impl PartialEq for DynSqVar {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Integer(l0), Self::Integer(r0)) => l0 == r0,
            (Self::Float(_), Self::Float(_)) => unimplemented!(),
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Table(_), Self::Table(_)) => unimplemented!(),
            (Self::Array(l0), Self::Array(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Eq for DynSqVar {}

impl PartialOrd for DynSqVar {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (DynSqVar::Null, DynSqVar::Null) => Some(Ordering::Equal),
            (DynSqVar::Integer(l), DynSqVar::Integer(r)) => l.partial_cmp(r),
            (DynSqVar::Float(_), DynSqVar::Float(_)) => None,
            (DynSqVar::Bool(l), DynSqVar::Bool(r)) => l.partial_cmp(r),
            (DynSqVar::String(l), DynSqVar::String(r)) => l.partial_cmp(r),
            (DynSqVar::Table(_), DynSqVar::Table(_)) => None,
            (DynSqVar::Array(l), DynSqVar::Array(r)) => l.partial_cmp(r),
            _ => None
        }
    }
}

impl Hash for DynSqVar {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            DynSqVar::Integer(i) => i.hash(state),
            DynSqVar::Float(_) => unimplemented!(),
            DynSqVar::Bool(b) => b.hash(state),
            DynSqVar::String(s) => s.hash(state),
            DynSqVar::Table(_) => unimplemented!(),
            DynSqVar::Array(a) => a.hash(state),
            DynSqVar::UserData(u) => u.hash(state),
            DynSqVar::Null => core::mem::discriminant(self).hash(state),
        }

    }
}


impl<VM> SqPush<DynSqVar> for VM
where 
    VM: SqVmApi + SqGet<SQNull> // TODO: Why this is working?
    
{
    fn push(&mut self, val: DynSqVar) -> Result<()> {
        match val {
            DynSqVar::Null => self.push(SQNull),
            DynSqVar::Integer(i) => self.push(i),
            DynSqVar::String(s) => self.push(s),
            DynSqVar::Array(v) => self.push(v),
            DynSqVar::Float(f) => self.push(f),
            DynSqVar::Bool(b) => self.push(b),
            DynSqVar::Table(t) => self.push(t),
            DynSqVar::UserData(u) => self.push(u),
        }
    }
}



impl<VM> SqGet<DynSqVar> for VM
where 
    VM: SqVmApi + SqGet<SQNull> 
{
    fn get(&mut self, idx: SQInteger) -> Result<DynSqVar> {
        match self.get_type(idx) {
            SqType::Null => Ok(DynSqVar::Null),
            SqType::Integer => Ok(DynSqVar::Integer(self.get(idx)?)),
            SqType::String => Ok(DynSqVar::String(self.get(idx)?)),
            SqType::Table => Ok(DynSqVar::Table(self.get(idx)?)),
            SqType::Array => Ok(DynSqVar::Array(self.get(idx)?)),
            SqType::Float => Ok(DynSqVar::Float(self.get(idx)?)),
            SqType::Bool => Ok(DynSqVar::Bool(self.get(idx)?)),
            SqType::UserData => Ok(DynSqVar::UserData(self.get(idx)?)),
            SqType::Unknown(t) => bail!("Unknown or unsupported type: 0x{t:X}"),
        }
    }
}



