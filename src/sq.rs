use anyhow::{bail, Context};
use squirrel2_kaleido_rs::*;
use std::{ptr::addr_of_mut, cmp::Ordering, collections::HashMap, hash::Hash};
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
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum SqType {
    Null,
    Integer,
    String,
    Array,
    Table,
    Float,
    Bool,

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
            unk => SqType::Unknown(unk)
        }
    }
}

/// Specifies VM behaviour on drop
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum VmSafety {
    /// Closed on drop
    Safe,
    /// Closed manually
    Unsafe,
    /// Uncloseable
    Friend
}   

/// Wrapper around SQVM handle. 
/// Can be [Safe](VmSafety::Safe),
/// [Unsafe](VmSafety::Unsafe),
/// or [Friend](VmSafety::Friend).
/// 
/// The only distinction is that Safe VM is closed on drop and Friend VM cannot be closed by user
pub struct SQVm {
    handle: HSQUIRRELVM,
    /// If [VmSafety::Safe], close VM on drop
    safety: VmSafety,
}

pub enum ExecState {
 
}

impl Drop for SQVm {
    fn drop(&mut self) {
        if VmSafety::Safe == self.safety { self._close() }
    }
}

/// VM
#[allow(unused)]
impl SQVm {
    /// Create struct from raw pointer to vm instance, by default [Unsafe](VmSafety::Unsafe) vm
    pub unsafe fn from_handle(handle: HSQUIRRELVM) -> Self {
        Self { handle, safety: VmSafety::Unsafe }
    }

    /// Creates a new instance of a squirrel VM that consists in a new execution stack.
    /// 
    /// Returns [Safe](VmSafety::Safe) VM
    pub fn open(initial_stack_size: usize) -> Self {
        Self {
            handle: unsafe { sq_open(initial_stack_size as _) },
            safety: VmSafety::Safe,
        }
    }
 
    /// Internal safe wrapper
    /// 
    /// Release a squirrel VM and all related friend VMs
    fn _close(&mut self) {
        unsafe { sq_close(self.handle) }
    }

    /// Release a squirrel VM and all related friend VMs 
    /// 
    /// Only for [Unsafe](VmSafety::Unsafe) VMs
    pub fn close(mut self) {
        match self.safety {
            VmSafety::Safe => panic!("Safe VM cannot be closed manually"),
            VmSafety::Unsafe => self._close(),
            VmSafety::Friend => panic!("Friend VMs cannot be closed"),
        }
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
    /// and pushes it in its stack as "thread" object.
    /// Returns [Friend](VmSafety::Friend) VM
    pub fn new_thread(&mut self, initial_stack_size: usize) -> Self {
        let handle = unsafe { sq_newthread(self.handle, initial_stack_size as _) };
        Self { handle, safety: VmSafety::Friend }
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

    
    /// Creates a new table and pushes it into the stack.
    pub fn new_table(&mut self) {
        unsafe { sq_newtable(self.handle) }
    }

    /// Pops a key and a value from the stack and performs a set operation
    /// on the table or class that is at position `idx` in the stack,
    /// if the slot does not exits it will be created.
    /// 
    /// if `is_static = true` creates a static member.
    /// This parameter is only used if the target object is a class
    pub fn new_slot(&mut self, idx: SQInteger, is_static: bool) -> Result<()> {
        sq_try! { self, unsafe { sq_newslot(self.handle, idx, is_static as _) } }
            .context(format!("Failed to create slot for table at idx {idx}"))
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
    pub unsafe fn push_float(&mut self, f: SQFloat) {
        sq_pushfloat(self.handle, f)
    }

    /// Gets the value of the float at the idx position in the stack.
    pub unsafe fn get_float(&mut self, idx: SQInteger) -> Result<SQFloat> {
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

impl<K, V> SqPush<HashMap<K, V>> for SQVm
where 
    SQVm: SqPush<K> + SqPush<V>,
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

impl<K, V> SqGet<HashMap<K, V>> for SQVm
where 
    SQVm: SqGet<K> + SqGet<V>,
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

impl SqPush<SQFloat> for SQVm {
    fn push(&mut self, val: SQFloat) -> Result<()> {
        unsafe { self.push_float(val) }
        Ok(())
    }
}

impl SqGet<SQFloat> for SQVm {
    fn get(&mut self, idx: SQInteger) -> Result<SQFloat> {
        unsafe { self.get_float(idx) }
    }
}

impl SqPush<bool> for SQVm {
    fn push(&mut self, val: bool) -> Result<()> {
        unsafe { self.push_bool(val) }
        Ok(())
    }
}

impl SqGet<bool> for SQVm {
    fn get(&mut self, idx: SQInteger) -> Result<bool> {
        unsafe { self.get_bool(idx) }
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
    //UserData(Vec<u8>),
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
            DynSqVar::Null => core::mem::discriminant(self).hash(state),
        }

    }
}

impl SqPush<DynSqVar> for SQVm {
    fn push(&mut self, val: DynSqVar) -> Result<()> {
        match val {
            DynSqVar::Null => self.push(SQNull),
            DynSqVar::Integer(i) => self.push(i),
            DynSqVar::String(s) => self.push(s),
            DynSqVar::Array(v) => self.push(v),
            DynSqVar::Float(f) => self.push(f),
            DynSqVar::Bool(b) => self.push(b),
            DynSqVar::Table(t) => self.push(t),
        }
    }
}

impl SqGet<DynSqVar> for SQVm {
    fn get(&mut self, idx: SQInteger) -> Result<DynSqVar> {
        match self.get_type(idx) {
            SqType::Null => Ok(DynSqVar::Null),
            SqType::Integer => Ok(DynSqVar::Integer(self.get(idx)?)),
            SqType::String => Ok(DynSqVar::String(self.get(idx)?)),
            SqType::Table => Ok(DynSqVar::Table(self.get(idx)?)),
            SqType::Array => Ok(DynSqVar::Array(self.get(idx)?)),
            SqType::Float => Ok(DynSqVar::Float(self.get(idx)?)),
            SqType::Bool => Ok(DynSqVar::Bool(self.get(idx)?)),
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


