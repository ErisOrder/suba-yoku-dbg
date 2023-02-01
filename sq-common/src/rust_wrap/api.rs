pub use squirrel2_kaleido_rs::*;

// Some re-exports to override SQIntegers as *size (they're always 32/64 anyways)
pub type SQInteger = isize;
pub type SQUnsignedInteger = usize;

pub type SQRESULT = SQInteger;
pub type SQBOOL = SQUnsignedInteger;

pub type SQCOMPILERERROR = Option<SqCompilerErrorHandler>;
pub type SQFUNCTION = Option<SqFunction>;
pub type SQRELEASEHOOK = Option<SqReleaseHook>;

/// Squirrel native closure underlying function
pub type SqFunction = extern "C" fn(vm: HSQUIRRELVM) -> SQInteger;

/// Hook that will be called on userdata release
pub type SqReleaseHook = extern "C" fn(
    p: SQUserPointer,
    size: SQInteger
) -> SQInteger;

/// Error handler that will be called in case of compiler error
pub type SqCompilerErrorHandler = extern "C" fn(
    vm: HSQUIRRELVM,
    desc: *const i8,
    src: *const i8,
    line: SQInteger,
    column: SQInteger,
);

/// Generate methods for vm api trait
macro_rules! to_method {
    // Custom name
    ($( $( #[$meta:meta] )* $new_name:ident -> $name:ident( $h:ident: HSQUIRRELVM $(,$arg:ident: $arg_type:ty)* ) $( -> $ret:ty )?; )+ ) => {
        $(
        $( #[$meta] )*
        #[inline]
        unsafe fn $new_name(&self $(,$arg:$arg_type)* ) $( -> $ret )? {
            $name(self.handle() $(,$arg  as _)* ) as _
        }
        )+
    };
    // Custom name and safe method
    ($( $( #[$meta:meta] )* safe $new_name:ident -> $name:ident( $h:ident: HSQUIRRELVM $(,$arg:ident: $arg_type:ty)* ) $( -> $ret:ty )?; )+ ) => {
        $(
        $( #[$meta] )*
        #[inline]
        fn $new_name(&self $(,$arg:$arg_type)* ) $( -> $ret )? {
            // allow arbitrary implicit conversions
            unsafe { $name(self.handle() $(,$arg as _)* ) as _ }
        }
        )+
    };
    // Most cases, translate name to sq_name
    ($( $( #[$meta:meta] )* $name:ident( $h:ident: HSQUIRRELVM $(,$arg:ident: $arg_type:ty)* ) $( -> $ret:ty )?; )+ ) => {
        $(
        $( #[$meta] )*
        #[inline]
        unsafe fn $name(&self $(,$arg:$arg_type)* ) $( -> $ret )? {
            //                                      Just to translate function types
            concat_idents!(sq_, $name)(self.handle() $(,std::mem::transmute($arg))* ) as _
        }
        )+
    };
}

/// Just a raw vm api wrapped into trait
pub trait VmRawApi {
    /// Expose handle to underlying VM
    fn handle(&self) -> HSQUIRRELVM;
    
    // Special case
    unsafe fn move_object(&self, dest: HSQUIRRELVM, idx: SQInteger) {
        sq_move(dest, self.handle(), idx as _)
    }

    // Safe renamed methods
    to_method! {
        /// Pops `count` elements from the stack
        safe pop -> sq_pop(v: HSQUIRRELVM, count: SQInteger);

        /// TODO: Check, how exactly this works
        ///
        /// Pushes copy(?) of an object at the `idx`
        safe clone_idx -> sq_push(v: HSQUIRRELVM, idx: SQInteger);

        /// Pushes a weak reference or copy in case of value object at the `idx` in the stack
        safe ref_idx -> sq_weakref(v: HSQUIRRELVM, idx: SQInteger);

        /// Removes an element from an arbitrary position in the stack
        safe remove -> sq_remove(v: HSQUIRRELVM, idx: SQInteger);

        /// Returns the index of the top of the stack
        safe stack_top -> sq_gettop(v: HSQUIRRELVM) -> SQInteger;

        /// Resize the stack, if new `top` is bigger then the current top the function will push nulls.
        safe set_stack_top -> sq_settop(v: HSQUIRRELVM, top: SQInteger);

        /// Ensure that the stack space left is at least of a specified `size`.
        /// If the stack is smaller it will automatically grow.
        safe reserve_stack -> sq_reservestack(v: HSQUIRRELVM, size: SQInteger);

        /// Compares 2 objects from the stack.
        safe stack_compare -> sq_cmp(v: HSQUIRRELVM) -> SQInteger;

        /// Get the type of the value at the position `idx` in the stack
        safe get_obj_type -> sq_gettype(v: HSQUIRRELVM, idx: SQInteger) -> SQObjectType;

        /// Creates a new array and pushes it into the stack.
        safe newarray -> sq_newarray(v: HSQUIRRELVM, size: SQInteger);

        /// Creates a new table and pushes it into the stack.
        safe new_table -> sq_newtable(v: HSQUIRRELVM);

        /// Pushes the current root table in the stack
        safe push_root_table -> sq_pushroottable(v: HSQUIRRELVM);

        /// Pushes a null value into the stack
        safe push_null -> sq_pushnull(v: HSQUIRRELVM);

        /// Get size of vm calls stack
        safe call_stack_len -> sq_getcallstacksize(v: HSQUIRRELVM) -> SQInteger;

        /// Pushes a integer into the stack
        safe push_integer -> sq_pushinteger(v: HSQUIRRELVM, n: SQInteger);

        /// Pushes a bool into the stack
        safe push_bool -> sq_pushbool(v: HSQUIRRELVM, b: bool);

        /// Pushes a float into the stack
        safe push_float -> sq_pushfloat(v: HSQUIRRELVM, f: SQFloat);

        /// Creates a new userdata and pushes it in the stack
        safe new_userdata -> sq_newuserdata(v: HSQUIRRELVM, size: SQUnsignedInteger) -> SQUserPointer;

        safe get_from_slot ->  sq_get(v: HSQUIRRELVM, idx: SQInteger) -> SQRESULT;
        safe set_to_slot -> sq_set(v: HSQUIRRELVM, idx: SQInteger) -> SQRESULT;
        
        /// Resets the last error in the virtual machine to null
        safe reset_error -> sq_reseterror(v: HSQUIRRELVM);

        /// In order to receive line callbacks the scripts have to be
        /// compiled with debug info enabled.
        safe enable_debug_info -> sq_enabledebuginfo(v: HSQUIRRELVM, enable: bool);
    }

    // Unsafe renamed methods
    to_method! {
        /// Copies and pushes a string into the stack
        /// # Safety
        /// Safe as long as `s` is pointer to memory region of size `len`
        push_string -> sq_pushstring(v: HSQUIRRELVM, s: *const u8, len: SQInteger);

        /// Pushes a userpointer into the stack
        /// # Safety
        /// When pointer goes to the vm, it can be used in different operations, including dereference
        push_userpointer -> sq_pushuserpointer(v: HSQUIRRELVM, p: SQUserPointer);
    }

    // Auto-named unsafe methods
    to_method!{
        newthread(friendvm: HSQUIRRELVM, initialstacksize: SQInteger) -> HSQUIRRELVM;
        seterrorhandler(v: HSQUIRRELVM);
        close(v: HSQUIRRELVM);
        setforeignptr(v: HSQUIRRELVM, p: SQUserPointer);
        getforeignptr(v: HSQUIRRELVM) -> SQUserPointer;
        setprintfunc(v: HSQUIRRELVM, printfunc: SQPRINTFUNCTION);
        getprintfunc(v: HSQUIRRELVM) -> SQPRINTFUNCTION;
        suspendvm(v: HSQUIRRELVM) -> SQRESULT;
        wakeupvm(
            v: HSQUIRRELVM,
            resumedret: SQBool,
            retval: SQBool,
            raiseerror: SQBool,
            throwerror: SQBool
        ) -> SQRESULT;
        getvmstate(v: HSQUIRRELVM) -> SQInteger;
        compile(
            v: HSQUIRRELVM,
            read: SQLEXREADFUNC,
            p: SQUserPointer,
            sourcename: *const SQChar,
            raiseerror: SQBool
        ) -> SQRESULT;
        compilebuffer(
            v: HSQUIRRELVM,
            s: *const SQChar,
            size: SQInteger,
            sourcename: *const SQChar,
            raiseerror: SQBool
        ) -> SQRESULT;
        notifyallexceptions(v: HSQUIRRELVM, enable: SQBool);
        setcompilererrorhandler(v: HSQUIRRELVM, f: SQCOMPILERERROR);
        poptop(v: HSQUIRRELVM);
        newclosure(v: HSQUIRRELVM, func: SQFUNCTION, nfreevars: SQUnsignedInteger);
        setparamscheck(
            v: HSQUIRRELVM,
            nparamscheck: SQInteger,
            typemask: *const SQChar
        ) -> SQRESULT;
        bindenv(v: HSQUIRRELVM, idx: SQInteger) -> SQRESULT;
        getsize(v: HSQUIRRELVM, idx: SQInteger) -> SQInteger;
        getbase(v: HSQUIRRELVM, idx: SQInteger) -> SQRESULT;
        instanceof(v: HSQUIRRELVM) -> SQBool;
        tostring(v: HSQUIRRELVM, idx: SQInteger);
        tobool(v: HSQUIRRELVM, idx: SQInteger, b: *mut SQBool);
        getstring(v: HSQUIRRELVM, idx: SQInteger, c: *mut *const SQChar) -> SQRESULT;
        getinteger(v: HSQUIRRELVM, idx: SQInteger, i: *mut SQInteger) -> SQRESULT;
        getfloat(v: HSQUIRRELVM, idx: SQInteger, f: *mut SQFloat) -> SQRESULT;
        getbool(v: HSQUIRRELVM, idx: SQInteger, b: *mut SQBool) -> SQRESULT;
        getthread(v: HSQUIRRELVM, idx: SQInteger, thread: *mut HSQUIRRELVM) -> SQRESULT;
        getuserpointer(v: HSQUIRRELVM, idx: SQInteger, p: *mut SQUserPointer) -> SQRESULT;
        getuserdata(
            v: HSQUIRRELVM,
            idx: SQInteger,
            p: *mut SQUserPointer,
            typetag: *mut SQUserPointer
        ) -> SQRESULT;
        settypetag(v: HSQUIRRELVM, idx: SQInteger, typetag: SQUserPointer) -> SQRESULT;
        gettypetag(v: HSQUIRRELVM, idx: SQInteger, typetag: *mut SQUserPointer) -> SQRESULT;
        setreleasehook(v: HSQUIRRELVM, idx: SQInteger, hook: SQRELEASEHOOK);
        getscratchpad(v: HSQUIRRELVM, minsize: SQInteger) -> *mut SQChar;
        getfunctioninfo(v: HSQUIRRELVM, idx: SQInteger, fi: *mut SQFunctionInfo) -> SQRESULT;
        getclosureinfo(
            v: HSQUIRRELVM,
            idx: SQInteger,
            nparams: *mut SQUnsignedInteger,
            nfreevars: *mut SQUnsignedInteger
        ) -> SQRESULT;
        setnativeclosurename(v: HSQUIRRELVM, idx: SQInteger, name: *const SQChar) -> SQRESULT;
        setinstanceup(v: HSQUIRRELVM, idx: SQInteger, p: SQUserPointer) -> SQRESULT;
        getinstanceup(
            v: HSQUIRRELVM,
            idx: SQInteger,
            p: *mut SQUserPointer,
            typetag: SQUserPointer
        ) -> SQRESULT;
        setclassudsize(v: HSQUIRRELVM, idx: SQInteger, udsize: SQInteger) -> SQRESULT;
        newclass(v: HSQUIRRELVM, hasbase: SQBool) -> SQRESULT;
        createinstance(v: HSQUIRRELVM, idx: SQInteger) -> SQRESULT;
        setattributes(v: HSQUIRRELVM, idx: SQInteger) -> SQRESULT;
        getattributes(v: HSQUIRRELVM, idx: SQInteger) -> SQRESULT;
        getclass(v: HSQUIRRELVM, idx: SQInteger) -> SQRESULT;
        getdefaultdelegate(v: HSQUIRRELVM, t: SQObjectType) -> SQRESULT;
        pushregistrytable(v: HSQUIRRELVM);
        pushconsttable(v: HSQUIRRELVM);
        setroottable(v: HSQUIRRELVM) -> SQRESULT;
        setconsttable(v: HSQUIRRELVM) -> SQRESULT;
        newslot(v: HSQUIRRELVM, idx: SQInteger, bstatic: SQBool) -> SQRESULT;
        deleteslot(v: HSQUIRRELVM, idx: SQInteger, pushval: SQBool) -> SQRESULT;
        rawget(v: HSQUIRRELVM, idx: SQInteger) -> SQRESULT;
        rawset(v: HSQUIRRELVM, idx: SQInteger) -> SQRESULT;
        rawdeleteslot(v: HSQUIRRELVM, idx: SQInteger, pushval: SQBool) -> SQRESULT;
        arrayappend(v: HSQUIRRELVM, idx: SQInteger) -> SQRESULT;
        arraypop(v: HSQUIRRELVM, idx: SQInteger, pushval: SQBool) -> SQRESULT;
        arrayresize(v: HSQUIRRELVM, idx: SQInteger, newsize: SQInteger) -> SQRESULT;
        arrayreverse(v: HSQUIRRELVM, idx: SQInteger) -> SQRESULT;
        arrayremove(v: HSQUIRRELVM, idx: SQInteger, itemidx: SQInteger) -> SQRESULT;
        arrayinsert(v: HSQUIRRELVM, idx: SQInteger, destpos: SQInteger) -> SQRESULT;
        setdelegate(v: HSQUIRRELVM, idx: SQInteger) -> SQRESULT;
        getdelegate(v: HSQUIRRELVM, idx: SQInteger) -> SQRESULT;
        clone(v: HSQUIRRELVM, idx: SQInteger) -> SQRESULT;
        setfreevariable(v: HSQUIRRELVM, idx: SQInteger, nval: SQUnsignedInteger) -> SQRESULT;
        next(v: HSQUIRRELVM, idx: SQInteger) -> SQRESULT;
        getweakrefval(v: HSQUIRRELVM, idx: SQInteger) -> SQRESULT;
        clear(v: HSQUIRRELVM, idx: SQInteger) -> SQRESULT;
        call(
            v: HSQUIRRELVM,
            params: SQInteger,
            retval: SQBool,
            raiseerror: SQBool
        ) -> SQRESULT;
        resume(v: HSQUIRRELVM, retval: SQBool, raiseerror: SQBool) -> SQRESULT;
        getlocal(
            v: HSQUIRRELVM,
            level: SQUnsignedInteger,
            idx: SQUnsignedInteger
        ) -> *const SQChar;
        getfreevariable(
            v: HSQUIRRELVM,
            idx: SQInteger,
            nval: SQUnsignedInteger
        ) -> *const SQChar;
        throwerror(v: HSQUIRRELVM, err: *const SQChar) -> SQRESULT;
        getlasterror(v: HSQUIRRELVM);
        getstackobj(v: HSQUIRRELVM, idx: SQInteger, po: *mut HSQOBJECT) -> SQRESULT;
        pushobject(v: HSQUIRRELVM, obj: HSQOBJECT);
        addref(v: HSQUIRRELVM, po: *mut HSQOBJECT);
        release(v: HSQUIRRELVM, po: *mut HSQOBJECT) -> SQBool;
        collectgarbage(v: HSQUIRRELVM) -> SQInteger;
        writeclosure(vm: HSQUIRRELVM, writef: SQWRITEFUNC, up: SQUserPointer) -> SQRESULT;
        readclosure(vm: HSQUIRRELVM, readf: SQREADFUNC, up: SQUserPointer) -> SQRESULT;
        stackinfos(v: HSQUIRRELVM, level: SQInteger, si: *mut SQStackInfos) -> SQRESULT;

        /// Pops a closure from the stack an sets it as debug hook
        ///
        /// In order to receive a 'per line' callback, is necessary
        /// to compile the scripts with theline informations.
        /// Without line informations activated, only the 'call/return' callbacks will be invoked.
        setdebughook(v: HSQUIRRELVM);

        closure_getinfos(v: HSQUIRRELVM, idx: SQInteger) -> SQRESULT;
    }
}