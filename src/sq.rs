use anyhow::{bail, Context};
use squirrel2_kaleido_rs::*;
use std::ptr::addr_of_mut;
use anyhow::Result;


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


pub trait SqVar where Self: Sized {
    /// Push value to stack
    unsafe fn sq_push(self, vm: HSQUIRRELVM) -> Result<()>;

    /// Retrieve value from stack at index (top is -1, bottom is 0)
    unsafe fn sq_get(vm: HSQUIRRELVM, idx: SQInteger) -> Result<Self>;  
}

pub trait SqError: ToString where Self: Sized {
    /// Throw to SQVM as string
    fn throw_to(self, vm: HSQUIRRELVM) {
        let mut msg = self.to_string();
        msg.push('\0');
        unsafe { sq_throwerror(vm, msg.as_ptr() as _); }
    }
}

impl SqError for anyhow::Error {
    fn throw_to(self, vm: HSQUIRRELVM) {
        let mut msg = self.root_cause().to_string();
        msg.push('\0');
        unsafe { sq_throwerror(vm, msg.as_ptr() as _); }
    }
}

#[derive(Debug, PartialEq)]
pub enum SqType {
    Null = tagSQObjectType_OT_NULL as _,
    Integer = tagSQObjectType_OT_INTEGER as _,
    String = tagSQObjectType_OT_STRING as _,
    Array = tagSQObjectType_OT_ARRAY as _,
}


impl TryInto<SqType> for SQObjectType {
    type Error = anyhow::Error;

    #[allow(non_upper_case_globals)]
    fn try_into(self) -> Result<SqType> {
        match self {
            tagSQObjectType_OT_NULL => Ok(SqType::Null),
            tagSQObjectType_OT_INTEGER => Ok(SqType::Integer),
            tagSQObjectType_OT_STRING => Ok(SqType::String),
            tagSQObjectType_OT_ARRAY => Ok(SqType::Array),
            _ => bail!("Unknown or unsupported type: 0x{self:x}")
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum DynSqVar {
    Null(SQNull),
    Integer(SQInteger),
    //Float(SQFloat),
    //Bool(SQBool),
    String(String),
    //Table(HashMap<DynSqVar, DynSqVar>),
    Array(Vec<DynSqVar>),
    //UserData(Vec<u8>),
    //UserPointer(SQInteger),
    //Class(SQClass),
    //Weakref(SQInteger)
} 

impl SqVar for DynSqVar {
    unsafe fn sq_push(self, vm: HSQUIRRELVM) -> Result<()> {
        match self {
            DynSqVar::Null(n) => n.sq_push(vm),
            DynSqVar::Integer(i) => i.sq_push(vm),
            DynSqVar::String(s) => s.sq_push(vm),
            DynSqVar::Array(a) => a.sq_push(vm),
        }
    }

    unsafe fn sq_get(vm: HSQUIRRELVM, idx: SQInteger) -> Result<Self> {
        match sq_gettype(vm, idx).try_into().context("Mismatched type")? {
            SqType::Null => 
                Ok(DynSqVar::Null(SQNull::sq_get(vm, idx)?)),
            SqType::Integer => 
                Ok(DynSqVar::Integer(SQInteger::sq_get(vm, idx)?)),
            SqType::String => 
                Ok(DynSqVar::String(String::sq_get(vm, idx)?)),
            SqType::Array =>
                Ok(DynSqVar::Array(Vec::sq_get(vm, idx)?)),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct SQNull;
impl SqVar for SQNull {
    unsafe fn sq_push(self, vm: HSQUIRRELVM) -> Result<()> {
        sq_pushnull(vm);
        Ok(())
    }

    unsafe fn sq_get(vm: HSQUIRRELVM, idx: SQInteger) -> Result<Self> {
        if SqType::Null != sq_gettype(vm, idx).try_into()? {
            bail!("Mismatched type of object at index {idx}: not null")
        } 
        Ok(Self)
    }
}


impl SqVar for SQInteger {
    unsafe fn sq_push(self, vm: HSQUIRRELVM) -> Result<()> {
        sq_pushinteger(vm, self);
        Ok(())
    }

    unsafe fn sq_get(vm: HSQUIRRELVM, idx: SQInteger) -> Result<Self> {
        let mut s: SQInteger = 0;
        let res = sq_getinteger(vm, idx, addr_of_mut!(s));
        if res != 0 { 
            bail!("Failed to get integer at idx {idx}") }
        else { Ok(s) }
    }
}

impl SqVar for &str {
    unsafe fn sq_push(self, vm: HSQUIRRELVM) -> Result<()> {
        sq_pushstring(vm, self.as_ptr() as _, self.len() as _);
        Ok(())
    }

    unsafe fn sq_get(_vm: HSQUIRRELVM, _idx: SQInteger) -> Result<Self> {
        unimplemented!("use String to get value from the vm")
    }
}

impl SqVar for String {
    unsafe fn sq_push(mut self, vm: HSQUIRRELVM) -> Result<()> {
        self.push('\0');
        sq_pushstring(vm, self.as_ptr() as _, -1);
        Ok(())
    }

    unsafe fn sq_get(vm: HSQUIRRELVM, idx: SQInteger) -> Result<Self> {
        let mut ptr = std::ptr::null_mut();
        
        let res = sq_getstring(vm, idx, addr_of_mut!(ptr) as _);
        if res != 0 { 
            bail!("Failed to get string at idx {idx}") }
        else {
            let len = libc::strlen(ptr);
            let mut v = Vec::with_capacity(len);
            std::ptr::copy(ptr, v.as_mut_ptr() as _, len);
            v.set_len(len);
            // TODO: Check if ptr to copied string or original
            Ok(String::from_utf8_unchecked(v)) }
    }
}

impl<T> SqVar for Vec<T> 
where 
    T: SqVar
{
    unsafe fn sq_push(self, vm: HSQUIRRELVM) -> Result<()> {
        sq_newarray(vm, self.len() as i32);

        for (index, elem) in self.into_iter().enumerate() {
            if let Err(e) = elem.sq_push(vm) {
                bail!("Failed to push element to stack: {e}")
            }

            // TODO: Retrieve proper error messages from SQVM
            if sq_arrayappend(vm, -2) != 0 {
                bail!("Failed to append element at index {index}")
            }
        }

        Ok(())
    }

    unsafe fn sq_get(vm: HSQUIRRELVM, idx: SQInteger) -> Result<Self> {
        // Push reference to array to the stack top
        sq_weakref(vm, idx);
        sq_pushnull(vm);

        let mut out = Vec::new();
        while sq_next(vm, -3) >= 0 {
            let elem = T::sq_get(vm, -1)
                .context("Failed to get array value")?;

            out.push(elem);

            sq_pop(vm, 2);
        }

        // Pop null iterator and weakref
        sq_pop(vm, 2);
        Ok(out)
    }
}

/// Binds generated SQ module to table
///
/// Sqrat function wrapping chain:
/// BindFunc(.., method) <- SqGlobalFunc<R>(method) <- template with needed argcount <- ~~static~~ fn(hsqvm) -> SQInteger  
#[macro_export]
macro_rules! sq_bind_method {
    ($bind_fn:expr, $tab_ptr:expr, $sq_mod:ident) => {
        {
            $bind_fn(
                $tab_ptr as _,
                concat!(stringify!($sq_mod), "\0").as_ptr(),
                $sq_mod::func as _,
                4,
                $sq_mod::sqfn as _,
                true
            );
        }
    };
}

// TODO: Implement attribute proc-macro
/// Generates module with function and it`s SQ wrapper
#[macro_export]
macro_rules! sq_gen_mod {
    ( $v:vis $name:ident ( $( $arg:ident: $atyp:ty ),* ) $( -> $rtyp:ty )? { $( $inner:tt )* }
    ) => {
        #[allow(unused_imports, non_snake_case)]
        $v mod $name {
            use std::ptr;
            use std::mem;
            use std::marker::PhantomData;

            use squirrel2_kaleido_rs::*;
            use $crate::sq::*;
            use log::debug;

            #[allow(unused_mut)]
            pub fn func($( mut $arg: $atyp, )*) $( -> $rtyp )? {
                $( $inner )*
            }

            #[allow(unreachable_code, unused_variables)]
            pub unsafe extern "cdecl" fn sqfn(hvm: HSQUIRRELVM) -> SQInteger 
            {
                // for some reason SQObject struct is 16  bytes in size, not 8
                // 0 is still type and 8 is SQObject, while 4 is 0xBAADFOOD and 12 is zeroed
                // this is fixed in custom version of SQ bindings 

                // FIXME: though it`s might be possible to retrieve method from userdata,
                // it is currently broken (maybe another struct needs to be changed...)

                //let mut method_ptr = ptr::null_mut::<libc::c_void>();
                //sq_getuserdata(hvm, -1, &mut method_ptr as _, ptr::null_mut());
                //let func: fn($($atyp,)*) $( -> $rtyp )? = mem::transmute(method_ptr);

                // pop unused userdata with method
                sq_pop(hvm, 1);

                let argc = ${ count(arg) };

                $(  
                    // index from -1 to -argc
                    let $arg = match <$atyp>::sq_get(hvm, - (${ index() } + 1) ) {
                        Ok(a) => a,
                        Err(e) => {
                            let e = e.context(format!("problem with argument {}", ${ index() }));
                            e.throw_to(hvm);
                            return -1;
                        }
                    };
                )*

                // Print arguments and their count
                debug!(target: stringify!($name),
                    concat!("argc: {}, args: ", $( stringify!($arg), " = {:?}; ", )* ),
                    argc, $( $arg ),*
                );

                let ret = func($( $arg ),*);

                // if return type exists, push it and return 1
                $( ${ ignore(rtyp) }
                    if let Err(e) = ret.sq_push(hvm) {
                        let e = e.context("failed to push return value");
                        e.throw_to(hvm);
                        return -1;
                    }
                    return 1;
                )? 

                0
            }
        }
    };
}
