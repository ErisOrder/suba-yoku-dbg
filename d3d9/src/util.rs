use std::time::Duration;

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
            std::thread::sleep(Duration::from_millis(15));
        }
    }
}


/// Bind a function and it's associated Squirrel closure to the object
/// 
/// ```cpp
/// // Source
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
    sq_fn: sq_common::SQFn,        // sq wrapper func
    static_var: bool    // for static member
);

/// Binds generated SQ module to table
///
/// Sqrat function wrapping chain:
/// BindFunc(.., method) <- SqGlobalFunc<R>(method) <- template with needed argcount <- ~~static~~ fn(hsqvm) -> SQInteger  
#[macro_export]
macro_rules! sq_bind_method {
    ($bind_fn:expr, $tab_ptr:expr, $sq_struct:ty) => {
        {
            $bind_fn(
                $tab_ptr as _,
                concat!(stringify!($sq_struct), "\0").as_ptr(),
                <$sq_struct>::rust_fn as _,
                4,
                <$sq_struct>::sq_fn as _,
                true
            );
        }
    };
}
