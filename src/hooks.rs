use dynasmrt::{dynasm, DynasmApi};
use region::Protection;
use anyhow::Result;

const CALL_SIZE: usize = 5;

const SQ_PRINTF_OFFSET: usize = 0x7AF0E;
const SQ_HOOK_SIZE: usize = 6;

const BASE_OFFSET: usize = 0x1000 - 0x400; // DataOffset - HeaderSize

unsafe fn fixup_addr(base_addr: usize, offset: usize) -> usize {
    base_addr + offset + BASE_OFFSET
}

unsafe fn place_call_hook(call_from: usize, call_to: usize, hook_size: usize) -> Result<()> {
    let call_addr = call_to - (call_from + CALL_SIZE);
    let nops = hook_size - CALL_SIZE;

    let mut ops = dynasmrt::x86::Assembler::new().unwrap();

    let offset = ops.offset();
    dynasm! { ops
        ; call call_addr as _
    };

    for _ in 0..nops {
        dynasm! { ops
            ; nop
        };
    }

    let buf = ops.finalize().unwrap();
    let ptr = buf.ptr(offset);

    region::protect(call_from as *const u8, 4096, Protection::READ_WRITE_EXECUTE)?;
    std::ptr::copy(ptr, call_from as *mut u8, buf.len());

    Ok(())
}

#[allow(clippy::fn_to_numeric_cast)]
pub unsafe fn hook_sq_printf(base_addr: usize) -> Result<()> {
    let mut asm_func = dynasmrt::x86::Assembler::new().unwrap();

    unsafe extern "stdcall" fn _print(s: *mut u8) {
        let len = libc::strlen(s as *const std::ffi::c_char);
        let sl = std::slice::from_raw_parts(s, len);
        let s = String::from_utf8_lossy(sl);
        print!("{s}");
    }

    // TODO: maybe make this a macros
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

    let from_offset = fixup_addr(base_addr, SQ_PRINTF_OFFSET);
    let to_offset = func_ptr as usize;

    place_call_hook(from_offset, to_offset, SQ_HOOK_SIZE)?;

    std::mem::forget(func_buf);

    Ok(())
}