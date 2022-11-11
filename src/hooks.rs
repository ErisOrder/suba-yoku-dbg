use std::sync::Mutex;
use dynasmrt::{dynasm, DynasmApi, AssemblyOffset};
use log::debug;
use region::Protection;
use anyhow::Result;

const CALL_SIZE: usize = 5;

const SQ_PRINTF_OFFSET: usize = 0x7AF0E;
const SQ_HOOK_SIZE: usize = 6;

const TEXT_HOOK_OFFSET: usize = 0xF3324;
const TEXT_HOOK_SIZE: usize = 5;

const BASE_OFFSET: usize = 0x1000 - 0x400; // DataOffset - HeaderSize

pub static TEXT_HOOK_ACTIVE: Mutex<bool> = Mutex::new(true);
pub static PRINTF_HOOK_ACTIVE: Mutex<bool> = Mutex::new(true);

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

unsafe fn _hook(base_addr: usize, hook_off: usize, hook_size: usize, asm: dynasmrt::x86::Assembler, lab: AssemblyOffset) -> Result<()> {
    let func_buf = asm.finalize().unwrap();
    let func_ptr = func_buf.ptr(lab);

    let from_offset = fixup_addr(base_addr, hook_off);
    let to_offset = func_ptr as usize;

    place_call_hook(from_offset, to_offset, hook_size)?;

    std::mem::forget(func_buf);

    Ok(())
}

macro_rules! prologue {
    ($ops:ident, $stack_size:expr) => {
        dynasm! { $ops
            ; .arch x86
            ; push ebp
            ; mov  ebp, esp
            ; sub  esp, $stack_size
            ; pushad
        };
    };
}

macro_rules! epilogue {
    ($ops:ident) => {
        dynasm! { $ops
            ; .arch x86
            ; popad
            ; mov  esp, ebp
            ; pop  ebp
        };
    };
}

macro_rules! _gen_hook {
    ( $hook_name:ident, $hook_off:expr, $hook_size:expr,
      asm { $($asm:tt)* },
      inner { $($inner:tt)* }
    ) => {
        #[allow(clippy::fn_to_numeric_cast)]
        pub unsafe fn $hook_name(base_addr: usize) -> Result<()> {
            $($inner)*
            let mut $hook_name = dynasmrt::x86::Assembler::new().unwrap();
            let func = $hook_name.offset();
            
            dynasm! { $hook_name
                $($asm)*
            };

            _hook(base_addr, $hook_off, $hook_size, $hook_name, func)
        }
    };
}

macro_rules! gen_hook {
    ( $hook_name:ident, $hook_off:expr, $hook_size:expr, $stack_size:expr,
      body  { $($asm:tt)* }
      inner { $($inner:tt)* }
    ) => {
        _gen_hook! { $hook_name, $hook_off, $hook_size, asm {
            ; .arch x86
            ;; prologue!($hook_name, $stack_size) 
            $($asm)*
            ;; epilogue!($hook_name)
            ; ret
        }, inner { $($inner)* } }
    };

    ( $hook_name:ident, $hook_off:expr, $hook_size:expr, $stack_size:expr,
      prolog { $($prolog:tt)* }
      body  { $($asm:tt)* }
      inner { $($inner:tt)* }
    ) => {
        _gen_hook! { $hook_name, $hook_off, $hook_size, asm {
            ; .arch x86
            $($prolog)*
            ;; prologue!($hook_name, $stack_size) 
            $($asm)*
            ;; epilogue!($hook_name)
            ; ret
        }, inner { $($inner)* } }
    };

    ( $hook_name:ident, $hook_off:expr, $hook_size:expr, $stack_size:expr,
      prolog { $($prolog:tt)* }
      body  { $($asm:tt)* }
      epilog { $($epilog:tt)* }
      inner { $($inner:tt)* }
    ) => {
        _gen_hook! { $hook_name, $hook_off, $hook_size, asm {
            ; .arch x86
            $($prolog)*
            ;; prologue!($hook_name, $stack_size) 
            $($asm)*
            ;; epilogue!($hook_name)
            $($epilog:tt)*
            ; ret
        }, inner { $($inner)* } }
    };

    ( $hook_name:ident, $hook_off:expr, $hook_size:expr, $stack_size:expr,
        body  { $($asm:tt)* }
        epilog { $($epilog:tt)* }
        inner { $($inner:tt)* }
    ) => {
        _gen_hook! { $hook_name, $hook_off, $hook_size, asm {
            ; .arch x86
            ;; prologue!($hook_name, $stack_size) 
            $($asm)*
            ;; epilogue!($hook_name)
            $($epilog:tt)*
            ; ret
        }, inner { $($inner)* } }
    };
}

gen_hook! {
    hook_sq_printf, SQ_PRINTF_OFFSET, SQ_HOOK_SIZE, 0x200,
    prolog {
        ; mov  eax, DWORD [eax + 0x124]
    }
    body {
        ; mov eax, [ebp + 12]
        ; push eax
        ; mov edx, DWORD _print as _
        ; call edx   
    }
    inner {
        unsafe extern "stdcall" fn _print(s: *mut u8) {
            if matches!(PRINTF_HOOK_ACTIVE.lock(), Ok(b) if *b) {
                let len = libc::strlen(s as *const std::ffi::c_char);
                let sl = std::slice::from_raw_parts(s, len);
                let s = String::from_utf8_lossy(sl);
                debug!(target: "printf_hook", "{s}");
            } 
        }
    }
}

gen_hook! {
    hook_text, TEXT_HOOK_OFFSET, TEXT_HOOK_SIZE, 0x200,
    prolog {
        ; add eax, DWORD [ebp + 0x30]
        // stack manipulation
        ; pop  ebx // ret addr temp
        ; pop  edi // 1st original pop
        ; pop  esi // 2nd original pop
        ; push ebx // restore ret aadr
    }
    body {        
        ; push eax
        ; mov edx, DWORD _print as _
        ; call edx   
    }
    inner {
        unsafe extern "stdcall" fn _print(s: *mut u8) {
            static mut PREV: Option<String> = None;
            if matches!(TEXT_HOOK_ACTIVE.lock(), Ok(b) if *b) {
                let len = libc::strlen(s as *const std::ffi::c_char);
                let sl = std::slice::from_raw_parts(s, len);
                let s = String::from_utf8_lossy(sl);
                
                // dedup
                if !matches!(&PREV, Some(prev_s) if prev_s.as_str() == s)  {
                    debug!(target: "text_hook", "{s}");
                    PREV = Some(s.to_string());
                } 
            }
        }
    }
}