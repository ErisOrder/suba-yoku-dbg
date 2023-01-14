#![cfg(windows)]
#![feature(asm_const)]

use std::time::Duration;

use sq_common::dbg;
use log::debug;
use winapi::shared::minwindef::{BOOL, DWORD, HINSTANCE, LPVOID, TRUE};

mod wrappers;
mod util;
mod hooks;

util_proc_macro::set_sqfn_paths!(sq_wrap_path = "sq_common");

dylib_mitm::dylib_mitm!("C:\\Windows\\SysWOW64\\d3d9.dll");

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

    ctrlc::set_handler(move || {
        let Some(ref dbg) = *hooks::SQ_DEBUGGER.lock().unwrap()
        else { return };

        if dbg.exec_state() == dbg::ExecState::Running {            
            println!("Execution halted");
            dbg.halt();
        }
    }).expect("failed to set ctrl+c handler");

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

    // Event Receiver thread
    std::thread::spawn(|| { 
        // Wait until debugger attached
        let recv = loop {
            if let Some(ref mut dbg) = *hooks::SQ_DEBUGGER.lock().unwrap() {
                break dbg.event_rx().clone();
            } else {
                std::thread::sleep(Duration::from_millis(50));
            }
        };

        // Debugger frontend thread
        // It`s spawned here, because event listener needs to be acquired first
        // due to frontend thread locking debugger mutex 
        std::thread::spawn(|| {
            let mut front = util::DebuggerFrontend::new();
            let mut arg_str =  String::new();
        
            println!("Debugger attached, type `help` to get available commands list");

            loop {
                std::thread::sleep(Duration::from_millis(20));
            
                let Some(ref mut dbg) = *hooks::SQ_DEBUGGER.lock().unwrap()
                else { unreachable!() };
            
                if let dbg::ExecState::Halted = dbg.exec_state() {
                    std::io::stdin().read_line(&mut arg_str).expect("failed to read cmd line");
                    
                    if !arg_str.trim().is_empty() {
                        match front.parse_args(&arg_str) {
                            Ok(_) => front.do_actions(dbg),
                            Err(e) => println!("{e}"),
                        };
                    } else { 
                        front.do_actions(dbg);
                    }
                
                    arg_str.clear();
                }
            }
        });

        // Print received events
        loop {
            if let Ok((e, bp)) = recv.recv() { 
                if let Some(bp) = bp {
                    println!("Reached debugger breakpoint {}", bp.number);
                }
                println!("{e}");
            }
        }   
    });
}


