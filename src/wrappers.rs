use user32::*;
use widestring::utf16str;
use winapi::{um::{
    sysinfoapi,
    consoleapi,
    wincon,
    wincontypes,
    winbase,
    wingdi,
    winnt::HANDLE, 
}, shared::minwindef::FALSE};

pub fn alloc_console() {
    unsafe {
        consoleapi::AllocConsole();

        // FIXME: Research on how to make this more clear
        let font = utf16str!("GulimChe");
        let mut fontname = [0u16; 32];
        std::ptr::copy(font.as_ptr(), fontname.as_mut_ptr(), font.len());

        wincon::SetConsoleOutputCP(65001);
        let mut font = wincon::CONSOLE_FONT_INFOEX {
            cbSize: std::mem::size_of::<wincon::CONSOLE_FONT_INFOEX>() as u32, 
            nFont: 0,
            dwFontSize: wincontypes::COORD { X: 0, Y: 20 },
            FontFamily: wingdi::FF_DONTCARE,
            FontWeight: 600,
            FaceName: fontname,
        };
        wincon::GetCurrentConsoleFontEx(
            winbase::STD_OUTPUT_HANDLE as HANDLE,
            FALSE,
            &mut font as *mut _
        );
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