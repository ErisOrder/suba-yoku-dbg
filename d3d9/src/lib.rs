#![cfg(windows)]
#![feature(abi_thiscall)]
#![feature(macro_metavar_expr)]

use winapi::shared::minwindef::{BOOL, DWORD, HINSTANCE, LPVOID, UINT, TRUE};
use winapi::shared::d3d9::IDirect3D9;
use lazy_static::lazy_static;

mod wrappers;
mod util;
mod hooks;
mod sq;

type D3D9CreateFn = unsafe extern "stdcall" fn(UINT) -> *mut IDirect3D9;

lazy_static! {
    static ref D3D9_LIB: libloading::Library = {
        println!("Loading D3D9 lib");
        let mut sys_dir = wrappers::get_system_directory();
        sys_dir.push_str("\\d3d9.dll");
        unsafe { libloading::Library::new(sys_dir).expect("failed to load d3d9 library") }
    };

    static ref D3D9_CREATE: libloading::Symbol<'static, D3D9CreateFn> = unsafe {
        D3D9_LIB.get(b"Direct3DCreate9").expect("failed to get Direct3DCreate9 address")
    };


}

/// Proxy function for passing call to real d3d9 lib
#[no_mangle]
#[allow(non_snake_case)]
extern "stdcall" fn Direct3DCreate9(sdk_version: UINT) -> *mut IDirect3D9 {
    unsafe { D3D9_CREATE(sdk_version) }
}

/// Based on https://github.com/rkarp/rust-dll-demo
/// 
/// Entry point which will be called by the system once the DLL has been loaded
/// in the target process. Declaring this function is optional.
///
/// # Safety
///
/// What you can safely do inside here is very limited, see the Microsoft documentation
/// about "DllMain". Rust also doesn't officially support a "life before main()",
/// though it is unclear what that that means exactly for DllMain.
#[no_mangle]
#[allow(non_snake_case, unused_variables)]
extern "system" fn DllMain(
    dll_module: HINSTANCE,
    call_reason: DWORD,
    reserved: LPVOID)
    -> BOOL
{
    const DLL_PROCESS_ATTACH: DWORD = 1;
    const DLL_PROCESS_DETACH: DWORD = 0;

    match call_reason {
        DLL_PROCESS_ATTACH => dll_init(),
        DLL_PROCESS_DETACH => (),
        _ => ()
    }

    TRUE
}

fn dll_init() {
    std::panic::set_hook(Box::new(|info| {
        let msg = format!("{}\n{}",
        &info.to_string(), std::backtrace::Backtrace::capture());
        wrappers::simple_message_box("Panic", &msg);
    }));

    std::env::set_var("RUST_LOG", "debug");
    pretty_env_logger::init();

    wrappers::alloc_console();

    unsafe {
        hooks::hook_sq_printf()
            .expect("failed to install hook");
        println!("printf hook installed");

        hooks::hook_text()
            .expect("failed to install hook");
        println!("text hook installed");

        hooks::hook_bind()
            .expect("failed to install hook");
        println!("bind hook installed");
    }

    std::thread::spawn(|| {
        let mut listener = util::KeyListener::new();

        listener.register_cb('T' as u16, || {
            if let Ok(mut b) = hooks::TEXT_HOOK_ACTIVE.lock() {
                *b = !*b;
                if *b { println!("text (strcpy?) hook active"); } 
                else { println!("text (strcpy?) hook disabled"); }
            }
        });

        listener.register_cb('H' as u16, || {
            if let Ok(mut b) = hooks::PRINTF_HOOK_ACTIVE.lock() {
                *b = !*b;
                if *b { println!("printf hook active"); } 
                else { println!("printf hook disabled"); }
            }
        });

        listener.register_cb('B' as u16, || {
            if let Ok(mut b) = hooks::BREAKPOINT_ACTIVE.lock() {
                *b = !*b;
                if *b { println!("breakpoint armed"); } 
                else { println!("breakpoint disarmed"); }
            }
        });

        listener.listen();
    });

}