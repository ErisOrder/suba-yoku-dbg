use std::sync::Mutex;
use dynasmrt::{dynasm, DynasmApi, AssemblyOffset};
use log::debug;
use region::Protection;
use anyhow::Result;
use lazy_static::lazy_static;

use crate::{sq, sq_gen_mod, wrappers, sq_bind_method};

const CALL_SIZE: usize = 5;

const SQ_PRINTF_OFFSET: usize = 0x7AF0E;
const SQ_HOOK_SIZE: usize = 6;

const TEXT_HOOK_OFFSET: usize = 0xF3324;
const TEXT_HOOK_SIZE: usize = 5;

const REG_FN_HOOK_OFFSET: usize = 0x9657B;
const REG_FN_HOOK_SIZE: usize = 5;

const SQ_BIND_FN_OFFSET: usize = 0xB5F0;

const BASE_OFFSET: usize = 0x1000 - 0x400; // DataOffset - HeaderSize

lazy_static! {
    static ref BASE_ADDR: usize = wrappers::get_base_mod_addr()
        .expect("failed to get base module (exe itself) address") as usize;
}

pub static TEXT_HOOK_ACTIVE: Mutex<bool> = Mutex::new(false);
pub static PRINTF_HOOK_ACTIVE: Mutex<bool> = Mutex::new(true);

pub fn fixup_addr(offset: usize) -> usize {
    *BASE_ADDR + offset + BASE_OFFSET
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

unsafe fn _hook(hook_off: usize, hook_size: usize, asm: dynasmrt::x86::Assembler, lab: AssemblyOffset) -> Result<()> {
    let func_buf = asm.finalize().unwrap();
    let func_ptr = func_buf.ptr(lab);

    let from_offset = fixup_addr(hook_off);
    let to_offset = func_ptr as usize;

    unsafe { 
        place_call_hook(from_offset, to_offset, hook_size)?;
    }

    std::mem::forget(func_buf);

    Ok(())
}

macro_rules! gen_hook {
    ( $hook_name:ident, $hook_off:expr, $hook_size:expr, $stack_size:expr,
      $( prolog { $( $prolog:tt )* } )?
      $( body  { $( $asm:tt )* } )?
      $( epilog { $( $epilog:tt )* } )?
      $( inner { $( $inner:tt )* } )?
    ) => {
        #[allow(clippy::fn_to_numeric_cast)]
        pub unsafe fn $hook_name() -> Result<()> {

            $( $( $inner )* )?

            let mut $hook_name = dynasmrt::x86::Assembler::new().unwrap();
            let func = $hook_name.offset();
            
            unsafe {
                dynasm! { $hook_name
                    ; .arch x86

                    $( $( $prolog )* )?

                    ; push ebp
                    ; mov  ebp, esp
                    ; sub  esp, $stack_size
                    ; pushad

                    $( $( $asm )* )?

                    ; popad
                    ; mov  esp, ebp
                    ; pop  ebp

                    $( $( $epilog )* )?

                    ; ret
                };

                _hook($hook_off, $hook_size, $hook_name, func)
            }
        }
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
                
                let s = if sl[sl.len() - 1] == b'\n' {
                    String::from_utf8_lossy(&sl[..sl.len() - 1])
                } else { String::from_utf8_lossy(sl) };
  
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

gen_hook! {
    // need to call 
    hook_bind, REG_FN_HOOK_OFFSET, REG_FN_HOOK_SIZE, 0x200,
    prolog {
        ; pop DWORD [&mut RET_ADDR as *mut usize as i32]
        ; mov DWORD [&mut SQ_TAB_PTR as *mut usize as i32], ecx
        ; mov eax, DWORD fixed_bind_fn as _
        ; call eax 
    }
    body {    
        ; push DWORD fixed_bind_fn as _
        ; mov edx, DWORD bind as _
        ; call edx       
    }
    epilog {
        ; push DWORD [&mut RET_ADDR as *mut usize as i32]
    }
    inner {
        static mut RET_ADDR: usize = 0;
        static mut SQ_TAB_PTR: usize = 0;

        let fixed_bind_fn = fixup_addr(SQ_BIND_FN_OFFSET);

        unsafe extern "stdcall" fn bind(func_: *const u8) {
            debug!(target: "bind_hook", "called stub, sq ptr: 0x{:X}", SQ_TAB_PTR);

            let bind_fn: sq::BindSQFnFn = std::mem::transmute(func_); 

            sq_bind_method!(bind_fn, SQ_TAB_PTR, SingleArg);
            sq_bind_method!(bind_fn, SQ_TAB_PTR, TestFunction);
            sq_bind_method!(bind_fn, SQ_TAB_PTR, TestArgs);
            sq_bind_method!(bind_fn, SQ_TAB_PTR, TestString);
            sq_bind_method!(bind_fn, SQ_TAB_PTR, TestDyn);
            sq_bind_method!(bind_fn, SQ_TAB_PTR, TestStaticArr);
            sq_bind_method!(bind_fn, SQ_TAB_PTR, TestVarargs);
            sq_bind_method!(bind_fn, SQ_TAB_PTR, TestTable);
        }
    }
}

sq_gen_mod! {
    TestFunction() -> SQInteger {
        777
    }

    SingleArg(a: SQInteger) -> SQInteger {
        a
    }

    TestArgs(a1: SQInteger, a2: SQInteger) -> SQInteger {
        a1 + a2
    }

    TestString(s: String) -> String {
        s.push_str(" + addition");
        s
    }

    TestDyn(d: DynSqVar) -> DynSqVar {
        let s = match d {
            DynSqVar::Null => "Null".into(),
            DynSqVar::Integer(i) => format!("Integer {i}"),
            DynSqVar::String(s) => format!("String {s}"),
            DynSqVar::Array(a) => format!("Array {a:?}"),
            DynSqVar::Float(f) => format!("Float {f}"),
            DynSqVar::Bool(b) => format!("Bool {b}"),
            DynSqVar::Table(t) => format!("Table {t:?}"),
        };
        debug!("received {s}");

        DynSqVar::Array(vec![
            DynSqVar::Integer(9),
            DynSqVar::Null,
            DynSqVar::String(String::from("Hello")),
        ])
    }

    TestStaticArr(a: Vec<SQInteger>) -> SQInteger {
        a.into_iter().sum()
    }

    TestVarargs(_norm: DynSqVar; varargs: ...) -> SQInteger {
        varargs.len() as _
    }

    TestTable(input: HashMap<DynSqVar, DynSqVar>) -> HashMap<DynSqVar, DynSqVar> {
        for (k, v) in input.into_iter() {
            debug!("table {k:?}: {v:?}");
        }

        let mut out = HashMap::new();
        out.insert(DynSqVar::Bool(false), DynSqVar::Bool(true));
        out.insert(DynSqVar::String("key".into()), DynSqVar::String("val".into()));
        out.insert(DynSqVar::Integer(2), DynSqVar::Integer(4));
        out.insert(DynSqVar::Array(vec![]), DynSqVar::Array(vec![]));

        out
    }
}
