#![cfg(windows)]

use std::time::Duration;

use log::debug;
use winapi::shared::minwindef::{BOOL, DWORD, HINSTANCE, LPVOID, TRUE};

mod wrappers;
mod util;
mod hooks;

dylib_mitm::dylib_mitm!(
    proto_path = r"C:\Windows\SysWOW64\d3d9.dll",
    load_lib = r#" &{
        let mut sys_dir = wrappers::get_system_directory();
        sys_dir.push_str(r"\d3d9.dll");
        sys_dir     
    }"#,
    manual_impls = "Direct3DCreate9",
);

#[allow(non_snake_case)]
#[dylib_mitm::manual_impl]
pub extern "C" fn Direct3DCreate9(version: u32) -> *mut u8 {
    debug!("Called Direct3DCreate9");
    unsafe { __Direct3DCreate9(version) }
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
    unsafe { d3d9::init(); }

    std::panic::set_hook(Box::new(|info| {
        let msg = format!("{}\n{}",
        &info.to_string(), std::backtrace::Backtrace::capture());
        wrappers::simple_message_box("Panic", &msg);
    }));

    wrappers::alloc_console();

    std::env::set_var("RUST_LOG", "debug");
    pretty_env_logger::init();

    unsafe {
        hooks::hook_sq_printf()
            .expect("failed to install hook");
        debug!("printf hook installed");

        hooks::hook_bind()
            .expect("failed to install hook");
        debug!("bind hook installed");

        hooks::hook_vm_init()
            .expect("failed to install hook");
        debug!("vm init hook installed");
    }

    // Debugger frontend thread
    std::thread::spawn(|| { 
        // Wait until debugger attached
        let dbg_middleware = loop {
            if let ref mut opt @ Some(_) = *hooks::SQ_DEBUGGER.lock().unwrap() {
                break opt.take().unwrap()
            }
            std::thread::sleep(Duration::from_millis(50));
        };

        util::DebuggerFrontend::connect(dbg_middleware)        
    });
}


