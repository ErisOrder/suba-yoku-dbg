use dynasmrt::{dynasm, DynasmApi};
use region::Protection;
use anyhow::Result;

const SQ_PRINTF_OFFSET: usize = 0x7AF0E;
const BASE_OFFSET: usize = 0x1000 - 0x400; // DataOffset - HeaderSize

/// NAKED void sq_printf_hook() {
///     __asm {
///         mov  eax, dword ptr ds : [eax + 0x124] //original
/// 
///         // alloc stack
///         push ebp;
///         mov  ebp, esp;
///         sub  esp, 0x200;
///         pushad; // save all regs
///     }
///     //================= USER CODE ================
///     char* p_str;
/// 
///     __asm {
///         mov eax,  [ebp + 12];
///         mov p_str, eax;
///     }
/// 
///     std::cout << p_str;
/// 
/// 
///     //============================================
///     __asm {
///         popad; // restore all regs
///         mov  esp, ebp;
///         pop  ebp;
///         ret;
///     }
/// }

#[allow(clippy::fn_to_numeric_cast)]
pub unsafe fn hook_sq_printf(base_addr: usize) -> Result<()> {
    let mut asm_func = dynasmrt::x86::Assembler::new().unwrap();
    let mut asm_hook = dynasmrt::x86::Assembler::new().unwrap();

    unsafe extern "stdcall" fn _print(s: *mut u8) {
        let len = libc::strlen(s as *const std::ffi::c_char);
        let sl = std::slice::from_raw_parts(s, len);
        let s = String::from_utf8_lossy(sl);
        print!("{s}");
    }

    let func = asm_func.offset();
    dynasm! { asm_func
        ; .arch x86
        ; mov  eax, DWORD [eax + 0x124]
        ; push ebp
        ; mov  ebp, esp
        ; sub  esp, 0x200
        ; pushad

        ; mov eax, [ebp + 12]
        ; push eax
        ; mov edx, DWORD _print as _
        ; call edx      

        ; popad
        ; mov  esp, ebp
        ; pop  ebp
        ; ret
    };

    let func_buf = asm_func.finalize().unwrap();
    let func_ptr = func_buf.ptr(func);

    // TODO: split call making to separate function
    let from_offset = base_addr + SQ_PRINTF_OFFSET + BASE_OFFSET;
    let to_offset = func_ptr as usize;

    let call_addr = to_offset - (from_offset + 5);

    let hook = asm_hook.offset();
    dynasm! { asm_hook
        ; .arch x86 
        ; call call_addr as _
        ; nop
    };

    let hook_buf = asm_hook.finalize().unwrap();
    let hook_ptr = hook_buf.ptr(hook);

    region::protect(from_offset as *const u8, 4096, Protection::READ_WRITE_EXECUTE)?;
    
    std::ptr::copy(hook_ptr, from_offset as *mut u8, hook_buf.len());

    std::mem::forget(func_buf);

    Ok(())
}