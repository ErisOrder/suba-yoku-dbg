use std::{sync::Mutex, ptr::addr_of_mut, sync::atomic::{self, AtomicBool}};
use std::collections::HashMap;
use dynasmrt::{dynasm, DynasmApi, AssemblyOffset};
use log::debug;
use region::Protection;
use anyhow::Result;
use lazy_static::lazy_static;
use util_proc_macro::{sqfn, sq_closure};
use sq_common::*;

use crate::wrappers;

const CALL_SIZE: usize = 5;

const SQ_PRINTF_OFFSET: usize = 0x7AF0E;
const SQ_HOOK_SIZE: usize = 6;

const REG_FN_HOOK_OFFSET: usize = 0x9657B;
const REG_FN_HOOK_SIZE: usize = 5;

const SQ_BIND_FN_OFFSET: usize = 0xB5F0;
const SQ_INIT_VM_FN_OFFSET:usize = 0x3E7A0;

const SQ_VM_INIT_HOOK_OFFSET: usize = 0x4B125;
const SQ_VM_INIT_HOOK_SIZE: usize = 5;

static mut SQVM_PTR: usize = 0;

const BASE_OFFSET: usize = 0x1000 - 0x400; // DataOffset - HeaderSize

lazy_static! {
    static ref BASE_ADDR: usize = wrappers::get_base_mod_addr()
        .expect("failed to get base module (exe itself) address") as usize;
}

pub static SQ_DEBUGGER: Mutex<Option<dbg::SqDebugger>> = Mutex::new(None); 

pub static PRINTF_HOOK_ACTIVE: AtomicBool = AtomicBool::new(true);

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
            if PRINTF_HOOK_ACTIVE.load(atomic::Ordering::Relaxed) {
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
    hook_vm_init, SQ_VM_INIT_HOOK_OFFSET, SQ_VM_INIT_HOOK_SIZE, 0x200,
    prolog {
        ; mov eax, fixup_addr(SQ_INIT_VM_FN_OFFSET) as _
        ; call eax
        ; mov DWORD [addr_of_mut!(SQVM_PTR) as _], eax
    }
    body {
        ; mov eax, vm_init as _
        ; call eax
    }
    inner {
        unsafe extern "stdcall" fn vm_init() {
            debug!(target: "vm_init_hook", "called vm_init(), sqvm ptr: {SQVM_PTR:X}");
        }
    }
}

gen_hook! {
    hook_bind, REG_FN_HOOK_OFFSET, REG_FN_HOOK_SIZE, 0x200,
    prolog {
        ; pop DWORD [addr_of_mut!(RET_ADDR) as _]
        ; mov DWORD [addr_of_mut!(SQ_TAB_PTR) as _], ecx
        ; mov eax, DWORD fixed_bind_fn as _
        ; call eax 
    }
    body {    
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
        
        // It might be possible to move this code to vm init hook,
        // but for some reason it isn`t working properly, maybe several vm threads involved
        unsafe extern "stdcall" fn bind() { 
            let mut vm = UnsafeVm::from_handle(SQVM_PTR as _).into_safe();

            vm.register_closure("TestClos", Box::new(|_vm| {
                debug!("Called closure");
                0
            }));

            let mut xx = 0;

            vm.register_closure("TestAutoGen", sq_closure!(
            #[(print_args = true)] 
            move |a: SqInteger| {
                xx += a;
                debug!("Called autogen closure: {a} {}", xx);
            }));

            vm.register_closure("Breakpoint", sq_closure!(
                || {
                    let Some(ref mut dbg) = *SQ_DEBUGGER.lock().unwrap()
                    else { return };

                    dbg.halt(true).unwrap();

                    // We can`t block vm thread to wait for message on vm thread
                    std::thread::spawn(|| {
                        let Some(ref mut dbg) = *SQ_DEBUGGER.lock().unwrap()
                        else { return };
                        let dbg::DebugResp::Event(e, _) = 
                            dbg.receiver().recv_timeout(std::time::Duration::from_millis(500)).unwrap()
                        else { panic!("expected event") };
                        println!("Reached native breakpoint:\n{e}");
                    });
                }
            ));

            register_test_functions(&mut vm);

            let dbg = dbg::SqDebugger::attach(vm);
            *SQ_DEBUGGER.lock().unwrap() = Some(dbg);
        }
    }
}

fn register_test_functions(vm: &mut SafeVm) {
    
    vm.register_function("TestFunction", test_function);
    vm.register_function("TestFunction", test_function);
    #[sqfn]
    fn test_function() -> SqInteger {
        777
    }

    vm.register_function("SingleArg", single_arg);
    #[sqfn]
    fn single_arg(a: SqInteger) -> SqInteger {
        a
    }

    vm.register_function("TestArgs", test_args);
    #[sqfn]
    fn test_args(a1: SqInteger, a2: SqInteger) -> SqInteger {
        a1 + a2
    }
    
    vm.register_function("TestString", test_string);
    #[sqfn]
    fn test_string(mut s: String) -> String {
        s.push_str(" + addition");
        s
    }
    
    vm.register_function("TestDyn", test_dyn);
    #[sqfn]
    fn test_dyn(d: DynSqVar) -> DynSqVar {
        let s = match d {
            DynSqVar::Null => "Null".into(),
            DynSqVar::Integer(i) => format!("Integer {i}"),
            DynSqVar::String(s) => format!("String {s}"),
            DynSqVar::Array(a) => format!("Array {a:?}"),
            DynSqVar::Float(f) => format!("Float {f}"),
            DynSqVar::Bool(b) => format!("Bool {b}"),
            DynSqVar::Table(t) => format!("Table {t:?}"),
            DynSqVar::UserData(u) => format!("UserData {u:?}"),
            DynSqVar::Unsupported(other) => format!("{other:?}")
        };
        debug!("received {s}");
        DynSqVar::Array(vec![
            DynSqVar::Integer(9),
            DynSqVar::Null,
            DynSqVar::String(String::from("Hello")),
        ])
    }
    
    vm.register_function("TestStaticArr", test_static_arr);
    #[sqfn]
    fn test_static_arr(a: Vec<SqInteger>) -> SqInteger {
        a.into_iter().sum()
    }
    
    vm.register_function("TestVarargs", test_varargs);
    #[sqfn(varargs = "varargs")]
    fn test_varargs(_norm: DynSqVar) -> SqInteger {
        varargs.len() as _
    }
    
    vm.register_function("TestTable", test_table);
    #[sqfn]
    fn test_table(input: HashMap<DynSqVar, DynSqVar>) -> HashMap<DynSqVar, DynSqVar> {
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
    
    vm.register_function("TestCreateUserData", test_create_userdata);
    #[sqfn]
    fn test_create_userdata() -> SqUserData {
        "magic_string".to_string().into_bytes().into()
    }
    
    vm.register_function("TestUserData", test_userdata);
    #[sqfn]
    fn test_userdata(inp: SqUserData) -> SqUserData {
        debug!("Received userdata: {inp:?}");
        inp
    }
    
    vm.register_function("TestOption", test_option);
    #[sqfn]
    fn test_option(s: Option<String>) -> String {
        match s {
            Some(s) => debug!("Received {s}"),
            None => debug!("Received null"),
        }
    
        "".into()
    } 

    vm.register_function("TestPushClosure", test_push_closure);
    #[sqfn] 
    fn test_push_closure() -> Box<SqFnClosure> {
        struct Indicator(i32);

        impl Drop for Indicator {
            fn drop(&mut self) {
                debug!("Dropping Indicator: {}", self.0);
            }
        }

        impl Indicator {
            pub fn addx(&mut self, x: SqInteger) {
                self.0 += x
            }
        }

        let mut capt = Indicator(42);

        sq_closure!(move |c: SqInteger| {
            debug!("called pushed closure: {}", capt.0);
            capt.addx(c);
        })
    }

    vm.register_function("TestHexdumpUserdata", test_hexdump_userdata);
    #[sqfn] 
    fn test_hexdump_userdata() -> Vec<Vec<SqUserData>> {
        let vec: Vec<_> = "hexdump test string that is 65 bytes, 5 rows in hexdump form long".as_bytes().into();
        vec![vec![vec.into()]]
    }
}