use anyhow::{bail, Context};
use squirrel2_kaleido_rs::*;
use std::{ptr::addr_of_mut, cmp::Ordering};
use anyhow::{
    Result,
    anyhow
};

macro_rules! sq_try {
    ($vm:expr, $e:expr) => {
        if { $e } != 0 {
            Err(anyhow!($vm.last_error()))
        } else { Ok(()) }
    };
}

/// Rust-adapted SQObjectType enum
#[derive(Debug, PartialEq)]
pub enum SqType {
    Null,
    Integer,
    String,
    Array,
    
    /// Stub for unknown or unsupported types
    Unknown(SQInteger)
}

#[derive(Debug, PartialEq)]
pub struct SQNull;

impl From<SQObjectType> for SqType {

    #[allow(non_upper_case_globals)]
    fn from(val: SQObjectType) -> Self {
        match val {
            tagSQObjectType_OT_NULL => SqType::Null,
            tagSQObjectType_OT_INTEGER => SqType::Integer,
            tagSQObjectType_OT_STRING => SqType::String,
            tagSQObjectType_OT_ARRAY => SqType::Array,
            unk => SqType::Unknown(unk)
        }
    }
}

/// Wrapper around SQVM handle. Does not close VM at drop
pub struct SQVm {
    handle: HSQUIRRELVM
}

pub enum ExecState {
 
}

/// VM
#[allow(unused)]
impl SQVm {
    /// Create struct from raw pointer to vm instance
    pub unsafe fn from_handle(handle: HSQUIRRELVM) -> Self {
        Self { handle }
    }

    /// Creates a new instance of a squirrel VM that consists in a new execution stack.
    pub fn open(initial_stack_size: usize) -> Self {
        Self { handle: unsafe { sq_open(initial_stack_size as _) } }
    }

    /// Release a squirrel VM and all related friend VMs
    pub fn close(self) {
        unsafe { sq_close(self.handle) }
    }

    /// Returns the execution state of a virtual machine
    pub fn get_vm_state(&self) -> ExecState {
        todo!()
    }

    /// Suspends the execution of the vm
    pub fn suspend(&mut self) -> Result<()> {
        sq_try! { self, unsafe { sq_suspendvm(self.handle) } }
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
    pub fn wake_up(&mut self, resumed_ret: bool, retval: bool, raise_err: bool, throw_err: bool) -> Result<()> {
        sq_try! { self, unsafe {
            sq_wakeupvm(self.handle, resumed_ret as _, retval as _, raise_err as _, throw_err as _)
        } }
    }

    /// Creates a new friend vm of this one  
    /// and pushes it in its stack as "thread" object
    pub fn new_thread(&mut self, initial_stack_size: usize) -> Self {
        let handle = unsafe { sq_newthread(self.handle, initial_stack_size as _) };
        Self { handle }
    }
}

/// Error handling and other
impl SQVm {
    /// Get last VM error
    pub fn last_error(&mut self) -> String {
        unsafe { sq_getlasterror(self.handle) }
        self.get(-1).expect("Failed to get last error")
    }

    /// Throw error string as an exception to the vm
    pub fn throw_error(&mut self, mut msg: String) {
        msg.push('\0');
        unsafe { sq_throwerror(self.handle, msg.as_ptr() as _); }
    }
}

/// Stack
#[allow(unused)]
impl SQVm {
    /// Pops `count` elements from the stack
    pub fn pop(&mut self, count: SQInteger) {
        unsafe { sq_pop(self.handle, count) }
    }

    // TODO: Check, how exactly this works
    /// Pushes copy(?) of an object at the `idx`
    pub fn clone_idx(&mut self, idx: SQInteger) {
        unsafe { sq_push(self.handle, idx) }
    }

    /// Pushes a weak reference or copy in case of value object at the `idx` in the stack
    pub fn ref_idx(&mut self, idx: SQInteger) {
        unsafe { sq_weakref(self.handle, idx) }
    }

    /// Removes an element from an arbitrary position in the stack
    pub fn remove(&mut self, idx: SQInteger) {
        unsafe { sq_remove(self.handle, idx) }
    }

    /// Returns the index of the top of the stack
    pub fn stack_len(&self) -> SQInteger {
        unsafe { sq_gettop(self.handle) } 
    }

    /// Resize the stack, if new `top` is bigger then the current top the function will push nulls.
    pub unsafe fn set_stack_top(&mut self, top: usize) {
        sq_settop(self.handle, top as _)
    }

    /// Ensure that the stack space left is at least of a specified `size`.
    /// If the stack is smaller it will automatically grow.
    pub fn reserve_stack(&mut self, size: SQInteger) {
        unsafe { sq_reservestack(self.handle, size) }
    } 

    /// Pushes the object at the position `idx` of the source vm stack in the destination vm stack
    pub fn move_obj(&mut self, to: &mut SQVm, idx: SQInteger) {
        unsafe { sq_move(to.handle, self.handle, idx) }
    }

    /// Compares 2 objects from the stack.
    pub fn stack_cmp(&self) -> Ordering {
        match unsafe { sq_cmp(self.handle) } {
            0.. => Ordering::Greater,
            0 => Ordering::Equal,
            SQInteger::MIN..=-1 => Ordering::Less,
        }
    }
}

/// Object manipulation
impl SQVm {
    /// Get the type of the value at the position `idx` in the stack
    #[inline]
    pub fn get_type(&self, idx: SQInteger) -> SqType {
        unsafe { sq_gettype(self.handle, idx) }.into()
    }

    /// Creates a new array and pushes it into the stack.
    pub fn new_array(&mut self, size: usize) {
        unsafe { sq_newarray(self.handle, size as _) }
    }

    /// Pops a value from the stack and pushes it in the back
    /// of the array at the position `idx` in the stack.
    pub fn array_append(&mut self, idx: SQInteger) -> Result<()> {
        sq_try! { self, unsafe { sq_arrayappend(self.handle, idx) } }
            .context(format!("Failed to insert value to array at index {idx}"))
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
    pub fn sq_iter_next(&mut self, idx: SQInteger) -> Result<()> {
        if unsafe { sq_next(self.handle, idx) } >= 0 {
            Ok(())
        } else {
            bail!("Failed to iterate using iterator at index {idx}")
        }
    }
}

/// Unsafe object manipulation
#[allow(unused)]
impl SQVm {
    /// Pushes a null value into the stack
    #[inline]
    pub unsafe fn push_null(&mut self) {
        sq_pushnull(self.handle)
    } 

    /// Copies and pushes a string into the stack
    #[inline]
    pub unsafe fn push_string(&mut self, s: *const u8, len: usize) {
        sq_pushstring(self.handle, s as _, len as _)
    }

    /// Get a pointer to the string at the `idx` position in the stack.
    pub unsafe fn get_string(&mut self, idx: SQInteger) -> Result<*const u8> {
        let mut ptr = std::ptr::null_mut();
        sq_try! { self,
            sq_getstring(self.handle, idx, addr_of_mut!(ptr) as _) 
        }.context(format!("Failed to get string at idx {idx}"))?;
        Ok(ptr)
    }

    /// Pushes a integer into the stack
    #[inline]
    pub unsafe fn push_integer(&mut self, int: SQInteger) {
        sq_pushinteger(self.handle, int)
    }

    // Get the value of the integer at the `idx` position in the stack.
    pub unsafe fn get_integer(&mut self, idx: SQInteger) -> Result<SQInteger> {
        let mut out = 0;
        sq_try! { self,
            sq_getinteger(self.handle, idx, addr_of_mut!(out)) 
        }.context(format!("Failed to get integer at idx {idx}"))?;
        Ok(out)
    }

    /// Pushes a bool into the stack
    #[inline]
    pub unsafe fn push_bool(&mut self, b: bool) {
        sq_pushbool(self.handle, b as _)
    }

    /// Get the value of the bool at the `idx` position in the stack.
    pub unsafe fn get_bool(&mut self, idx: SQInteger) -> Result<bool> {
        let mut out = 0;
        sq_try! { self,
            sq_getbool(self.handle, idx, addr_of_mut!(out)) 
        }.context(format!("Failed to get bool at idx {idx}"))?;
        Ok(out != 0)
    }

    /// Pushes a float into the stack
    #[inline]
    pub unsafe fn push_float(&mut self, f: f32) {
        sq_pushfloat(self.handle, f)
    }

    /// Gets the value of the float at the idx position in the stack.
    pub unsafe fn get_float(&mut self, idx: SQInteger) -> Result<f32> {
        let mut out = 0.0;
        sq_try! { self,
            sq_getfloat(self.handle, idx, addr_of_mut!(out)) 
        }.context(format!("Failed to get float at idx {idx}"))?; 
        Ok(out)
    }
}


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


impl SqThrow<anyhow::Error> for SQVm {
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

impl SqPush<SQInteger> for SQVm {
    fn push(&mut self, val: SQInteger) -> Result<()> {
        unsafe { self.push_integer(val) }
        Ok(())
    }
}

impl SqGet<SQInteger> for SQVm {
    fn get(&mut self, idx: SQInteger) -> Result<SQInteger> {
        unsafe { self.get_integer(idx) }
    }
}

impl SqPush<String> for SQVm {
    fn push(&mut self, val: String) -> Result<()> {
        unsafe { self.push_string(val.as_ptr(), val.len() as _) }
        Ok(())
    }
}

impl SqPush<&str> for SQVm {
    fn push(&mut self, val: &str) -> Result<()> {
        unsafe { self.push_string(val.as_ptr(), val.len() as _) }
        Ok(())
    }
}

impl SqGet<String> for SQVm {
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
impl SqPush<SQNull> for SQVm {
    fn push(&mut self, _: SQNull) -> Result<()> {
        unsafe { self.push_null(); }
        Ok(())
    }
}

/// For type safety
impl SqGet<SQNull> for SQVm {
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

impl<T> SqPush<Vec<T>> for SQVm
where 
    SQVm: SqPush<T>
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

impl<T> SqGet<Vec<T>> for SQVm
where 
    SQVm: SqGet<T>
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

#[derive(Debug, PartialEq)]
pub enum DynSqVar {
    Null,
    Integer(SQInteger),
    //Float(f32),
    //Bool(bool),
    String(String),
    //Table(HashMap<DynSqVar, DynSqVar>),
    Array(Vec<DynSqVar>),
    //UserData(Vec<u8>),
    //UserPointer(???),
    //Class(SQClass),
    //Weakref(???)
} 

impl SqPush<DynSqVar> for SQVm {
    fn push(&mut self, val: DynSqVar) -> Result<()> {
        match val {
            DynSqVar::Null => self.push(SQNull),
            DynSqVar::Integer(i) => self.push(i),
            DynSqVar::String(s) => self.push(s),
            DynSqVar::Array(v) => self.push(v),
        }
    }
}

impl SqGet<DynSqVar> for SQVm {
    fn get(&mut self, idx: SQInteger) -> Result<DynSqVar> {
        match self.get_type(idx) {
            SqType::Null => Ok(DynSqVar::Null),
            SqType::Integer => Ok(DynSqVar::Integer(self.get(idx)?)),
            SqType::String => Ok(DynSqVar::String(self.get(idx)?)),
            SqType::Array => Ok(DynSqVar::Array(self.get(idx)?)),
            SqType::Unknown(t) => bail!("Unknown or unsupported type: 0x{t:X}"),
        }
    }
}


/// C SQFunction type 
pub type SQFn = unsafe extern "cdecl" fn(HSQUIRRELVM) -> SQInteger;

/// Bind a function and it's associated Squirrel closure to the object
/// 
/// ```cpp
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
    sq_fn: SQFn,        // sq wrapper func
    static_var: bool    // for static member
);


