use anyhow::bail;
use squirrel2_rs::*;
use std::ptr::addr_of_mut;
use anyhow::Result;
use lazy_static::lazy_static;
use crate::hooks;

const SQ_THROWERROR_OFFSET: usize = 0x41340;

lazy_static! {
    pub static ref SQ_THROWERROR_FIXED: SQThrowErrorFn = {
        let addr = hooks::fixup_addr(SQ_THROWERROR_OFFSET);
        unsafe { std::mem::transmute(addr) }
    };
}

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
pub type SQThrowErrorFn = unsafe extern "cdecl" fn(HSQUIRRELVM, *mut u8);

pub type SQFn = unsafe extern "cdecl" fn(HSQUIRRELVM) -> SQInteger;

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
    unsafe fn push(self, vm: HSQUIRRELVM);

    /// Retrieve value from stack at index (top is -1, bottom is 0)
    unsafe fn get(vm: HSQUIRRELVM, idx: SQInteger) -> Result<Self>;  
}

impl SqVar for SQInteger {
    unsafe fn push(self, vm: HSQUIRRELVM) {
        sq_pushinteger(vm, self);
    }

    unsafe fn get(vm: HSQUIRRELVM, idx: SQInteger) -> Result<Self> {
        let mut s: SQInteger = 0;
        let res = sq_getinteger(vm, idx, addr_of_mut!(s));
        if res != 0 { 
            bail!("Failed to get element at idx {idx}") }
        else { Ok(s) }
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


/// Generates function with it`s SQ wrapper
#[macro_export]
macro_rules! sq_gen_func {
    ( $v:vis $name:ident ( $( $arg:ident: $atyp:ty ),* ) $( -> $rtyp:ty )? { $( $inner:tt )* }
    ) => {
        #[allow(unused_imports, non_snake_case)]
        $v mod $name {
            use std::ptr;
            use std::mem;
            use std::marker::PhantomData;

            use squirrel2_rs::*;
            use $crate::sq::*;
            use log::debug;

            pub fn func($( $arg: $atyp, )*) $( -> $rtyp )? {
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
                //let method: fn($($atyp,)*) -> $rtyp = mem::transmute(method_ptr);

                // pop unused userdata with method
                sq_pop(hvm, 1);

                let argc = ${ count(arg) };

                $(  
                    // index from -1 to -argc
                    let $arg = match <$atyp>::get(hvm, - (${ index() } + 1) ) {
                        Ok(a) => a,
                        Err(e) => {
                            let mut msg = e.to_string();
                            msg.push('\0');
                            // FIXME: Research why lib function don`t work
                            SQ_THROWERROR_FIXED(hvm, msg.as_ptr() as _); 
                            // Intended memory leak as sq_throwerror expects static str
                            mem::forget(msg);
                            return -1;
                        }
                    };
                )*

                // Print arguments and their count
                debug!(target: stringify!($name),
                    concat!("argc: {}, args: ", $( stringify!($arg), " = {}; ", )* ),
                    argc, $( $arg ),*
                );

                let ret = func($( $arg ),*);

                // if return type exists, push it and return 1
                $( ${ ignore(rtyp) }
                    ret.push(hvm);
                    return 1;
                )? 

                0
            }
        }
    };
}
