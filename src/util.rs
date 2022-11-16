use keyboard_query::{DeviceQuery, DeviceState};

pub struct KeyListener {
    callbacks: Vec<(u16, Box<dyn Fn()>)>
}

impl KeyListener {
    pub fn new() -> Self {
        Self { callbacks: vec![] }
    }

    pub fn register_cb<F: Fn() + 'static>(&mut self, key: u16, f: F) {
        self.callbacks.push((key, Box::new(f)));
    }

    pub fn listen(&self) -> ! {
        let device_state = DeviceState::new();
        let mut prev_keys = vec![];

        loop {
            let keys = device_state.get_keys();
            if keys != prev_keys {
                for key in keys.iter() {
                    let cb = self.callbacks.iter().find(|(k, _)| *k == *key);
                    if let Some(cb) = cb { cb.1() }
                }
            }
            prev_keys = keys;
        }
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
    ( $( $v:vis $name:ident ( $( $arg:ident: $atyp:ty ),* $(;$varargs:ident: ...)? ) $( -> $rtyp:ty )? { $( $inner:tt )* } )+ 
    ) => {
        $(
        #[allow(unused_imports, non_snake_case)]
        $v mod $name {
            use squirrel2_kaleido_rs::*;
            use $crate::sq::*;
            use log::debug;

            #[allow(unused)]
            type VarArgs = Vec<DynSqVar>;

            #[allow(unused_mut)]
            pub fn func($( mut $arg: $atyp, )* $( $varargs: VarArgs )? ) $( -> $rtyp )? {
                $( $inner )*
            }

            #[allow(unreachable_code, unused_variables)]
            pub unsafe extern "cdecl" fn sqfn(hvm: HSQUIRRELVM) -> SQInteger 
            {
                let mut vm = SQVm::from_handle(hvm);
                // for some reason SQObject struct is 16  bytes in size, not 8
                // 0 is still type and 8 is SQObject, while 4 is 0xBAADFOOD and 12 is zeroed
                // this is fixed in custom version of SQ bindings 

                // FIXME: though it`s might be possible to retrieve method from userdata,
                // it is currently broken (maybe another struct needs to be changed...)

                //let mut method_ptr = ptr::null_mut::<libc::c_void>();
                //sq_getuserdata(hvm, -1, &mut method_ptr as _, ptr::null_mut());
                //let func: fn($($atyp,)*) $( -> $rtyp )? = mem::transmute(method_ptr);

                // pop unused userdata with method
                vm.pop(1);

                // Stack layout (class method with 2 args): 
                // 1: this TODO: Check if all functions has class or table instance
                // 2: arg0
                // 3: arg1 <-- top
                // ?: popped userdata with method ptr 
                // technically, all functions has varargs by default

                let top = vm.stack_len();
                let norm_argc = ${ count(arg) };

                $(  // normal (rust) args indexes: 2..2+norm_argc
                    let idx = ${ index() } + 2;
                    let $arg: $atyp = match vm.get(idx) {
                        Ok(a) => a,
                        Err(e) => {
                            vm.throw(e.context(
                                format!("problem with argument {idx}")
                            ));
                            return -1;
                    }};
                )*

                $(  // vararg (rust) indexes: norm_argc+2..=top
                    let mut $varargs: VarArgs = vec![];
                    for i in norm_argc+2..=top {
                        let val: DynSqVar = match vm.get(i) {
                            Ok(a) => a,
                            Err(e) => {
                                vm.throw(e.context(
                                    format!("problem with vararg {i}")
                                ));
                                return -1;
                        }};
                        $varargs.push(val);
                    }
                )?

                // Print arguments and their count
                debug!(target: stringify!($name),
                    concat!("argc: {}, args: this; ", $( stringify!($arg), " = {:?}; ", )* $( stringify!($varargs), " = {:?}" )? ),
                    top, $( $arg ),* $(, $varargs )?
                );

                let ret = func($( $arg, )* $( $varargs )?);

                // if return type exists, push it and return 1
                $( ${ ignore(rtyp) }
                    if let Err(e) = vm.push(ret) {
                        vm.throw(e.context("failed to push return value"));
                        return -1;
                    }
                    return 1;
                )? 

                0
            }
        } 
        )+
    };
}
