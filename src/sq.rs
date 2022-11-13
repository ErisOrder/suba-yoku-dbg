use squirrel2_rs::*;

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
    unsafe fn push(self, vm: HSQUIRRELVM);
}

impl SqVar for SQInteger {
    unsafe fn push(self, vm: HSQUIRRELVM) {
        sq_pushinteger(vm, self);
    }
}


/// BindFunc(.., method) <- SqGlobalFunc<R>(method) <- template with needed argcount <- ~~static~~ fn(hsqvm) -> SQInteger  
/// 
#[macro_export]
macro_rules! sq_gen_func {
    // TODO: add visibility mods
    ( $name:ident ($($arg:ident: $atyp:ty,)*) -> $rtyp:ty { $($inner:tt)* }
    ) => {
        mod $name {
            use std::ptr;
            use std::mem;

            use squirrel2_rs::*;
            use $crate::sq::*;
            use log::debug;

            pub fn func($($arg: $atyp,)*) -> $rtyp {
                $($inner)*
            }
    
            // FIXME: currently only 0 args
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

                let ret: $rtyp = func();
                ret.push(hvm);
                1
            }
        }
    };
}
