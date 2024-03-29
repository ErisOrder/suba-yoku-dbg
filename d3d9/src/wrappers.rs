use user32::*;
use winapi::{um::{
    sysinfoapi,
    consoleapi,
    wincon,
    psapi,
    processthreadsapi as ptapi,
}, shared::minwindef::{HMODULE, DWORD}};
use anyhow::Result;

/// Call winapi function and return handle or error
macro_rules! handle_or_err {
    ($e:expr) => {
        {
            let handle = $e;
            if handle as usize == 0 {
                Err(std::io::Error::last_os_error())
            } else {
                Ok(handle)
            }
        }
    };
}

/// Allocate console and redirect stdout and stdin to it
pub fn alloc_console() {
    unsafe {
        consoleapi::AllocConsole();
        wincon::SetConsoleOutputCP(65001);
    }

}

pub fn simple_message_box(title: &str, text: &str) {

    let mut title: Vec<u16> = title.encode_utf16().collect();
    let mut text: Vec<u16> = text.encode_utf16().collect();

    title.push(0);
    text.push(0);

    unsafe {
        MessageBoxW(
            std::ptr::null_mut(),
            text.as_ptr(),
            title.as_ptr(),
            0
        );
    }
}

pub fn get_system_directory() -> String {
    let mut buff = Vec::with_capacity(256);
    unsafe { 
        let len = sysinfoapi::GetSystemDirectoryA(buff.as_mut_ptr() as *mut i8, 256);
        buff.set_len(len as usize);
        String::from_utf8_unchecked(buff)
    }
}

pub fn get_base_mod_addr() -> Result<*mut u8> {

    let proc = handle_or_err! { unsafe { ptapi::GetCurrentProcess() } }?;

    let mut handles = [0 as HMODULE; 256];
    let mut lpcb_needed: DWORD = 0;

    handle_or_err! { unsafe { psapi::EnumProcessModules(
        proc,
        handles.as_mut_ptr(),
        (std::mem::size_of::<HMODULE>() * 256) as u32,
        &mut lpcb_needed as *mut DWORD
    ) }}?;
    
    Ok(handles[0] as *mut u8)
}