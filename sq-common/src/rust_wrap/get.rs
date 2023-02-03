use indexmap::IndexMap;
use std::hash::Hash;


use crate::{error::*, util::cstr_to_string};
use super::types::*;
use super::vm::{Vm, safety::VmDrop};
use super::push::{SqPush, IntoPushResult};

// temp stub
macro_rules! sq_validate {
    ($check:expr, $($valid_type:path),+) => {
        match $check {
            $(| $valid_type )+ => Ok(()),
            other => Err($crate::error::SqVmError::Other(None))
        } 
    }
}

pub type SqGetResult<T> = Result<T, SqStackError>;

/// Trait for getting values from the vm stack
pub trait SqGet<T> {
    /// Get value from the vm stack at position `idx`.
    /// 
    /// if `idx` < 0, count from the top, else from the stack bottom.
    /// 
    /// Limit containers' recursion with `max_depth`.
    /// 
    /// This method mainly exists to prevent eternal recursion of self-referential containers.
    fn get_constrain(&self, idx: SqInteger, max_depth: Option<u32>) -> SqGetResult<T>;
     
    /// Get value from the vm stack at position `idx`
    /// 
    /// if `idx` < 0, count from the top, else from the stack bottom
    /// 
    /// Do not limit recursion
    fn get(&self, idx: SqInteger) -> SqGetResult<T> {
        self.get_constrain(idx, None)
    }
}

impl<S> SqGet<SqUnit> for Vm<S> where S: VmDrop {
    fn get_constrain(&self, _: SqInteger, _: Option<u32>) -> SqGetResult<SqUnit> {
        Ok(SqUnit)
    }
}

impl<S> SqGet<SqNull> for Vm<S> where S: VmDrop {
    #[inline]
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> SqGetResult<SqNull> {
        sq_validate!(self.get_type(idx), SqType::Null)
            .map_err(|e| e.into_stack_error("failed to get null"))?;
        Ok(SqNull)
    }
}

impl<S> SqGet<bool> for Vm<S> where S: VmDrop {
    #[inline]
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> SqGetResult<bool> {
        sq_validate!(self.get_type(idx), SqType::Bool)
            .map_err(|e| e.into_stack_error("failed to get bool"))?;
        Ok(self.get_bool(idx).unwrap())
    }
}

impl<S> SqGet<SqInteger> for Vm<S> where S: VmDrop {
    #[inline]
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> SqGetResult<SqInteger> {
        sq_validate!(self.get_type(idx), SqType::Integer)
            .map_err(|e| e.into_stack_error("failed to get integer"))?;
        Ok(self.get_integer(idx).unwrap())
    }
}

impl<S> SqGet<SqFloat> for Vm<S> where S: VmDrop {
    #[inline]
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> SqGetResult<SqFloat> {
        sq_validate!(self.get_type(idx), SqType::Float)
            .map_err(|e| e.into_stack_error("failed to get float"))?;
        Ok(self.get_float(idx).unwrap())
    }
}

impl<S, T> SqGet<SqUserPointer<T>> for Vm<S> where S: VmDrop {
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> SqGetResult<SqUserPointer<T>> {
        sq_validate!(self.get_type(idx), SqType::UserPointer)
            .map_err(|e| e.into_stack_error("failed to get userpointer"))?;
        
        Ok(self.get_userpointer(idx).map(|p| p as *mut T).unwrap())
    }
}

// TODO: Check encoding
// TODO: Use get_size() to make this more safe
impl<S> SqGet<String> for Vm<S> where S: VmDrop {
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> SqGetResult<String> {
        sq_validate!(self.get_type(idx), SqType::String)
            .map_err(|e| e.into_stack_error("failed to get string"))?;
        unsafe {
            let ptr = self.get_string(idx).unwrap();  
            Ok(cstr_to_string(ptr as _))
        }
    }
}

impl<S> SqGet<SqUserData> for Vm<S> where S: VmDrop {
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> SqGetResult<SqUserData> {
        sq_validate!(self.get_type(idx), SqType::UserData)
            .map_err(|e| e.into_stack_error("failed to get userdata"))?;
        
        // TODO: Add typetag to UserData struct
        let (user_data, _) = self.get_userdata(idx).unwrap(); 
        let out = unsafe {
            let size = self.get_size(idx)
                .map_err(|e| e.into_stack_error("failed to get size of userdata"))?
                 as usize;
            let mut out = Vec::with_capacity(size);
            std::ptr::copy(user_data, out.as_ptr() as _, size);
            out.set_len(size);
            SqUserData(out)
        };
        Ok(out)
    }
}

impl<S, T> SqGet<Option<T>> for Vm<S> 
where
    S: VmDrop,
    Vm<S>: SqGet<T> 
{
    fn get_constrain(&self, idx: SqInteger, max_depth: Option<u32>) -> SqGetResult<Option<T>> {
        match self.get_type(idx) {
            SqType::Null => Ok(None),
            _ => Ok(Some(SqGet::<T>::get_constrain(self, idx, max_depth)?))
        }
    }
}

impl<S, T> SqGet<Vec<T>> for Vm<S> 
where
    S: VmDrop,
    Vm<S>: SqGet<T> 
{
    fn get_constrain(&self, idx: SqInteger, max_depth: Option<u32>) -> SqGetResult<Vec<T>> {
        sq_validate!(self.get_type(idx), SqType::Array)
            .map_err(|e| e.into_stack_error("failed to get array"))?;
        
        if matches!(max_depth, Some(depth) if depth == 0) {
            return Ok(vec![]);
        }

        let mut out = vec![];

        for elem in self.iter_array(idx, max_depth) {
            out.push(elem?);
        }

        Ok(out)
    }
}

impl<S, K, V> SqGet<IndexMap<K, V>> for Vm<S>
where 
    S: VmDrop,
    K: PartialEq + Eq + Hash,
    Vm<S>: SqGet<K> + SqGet<V>,
{
    fn get_constrain(
        &self,
        idx: SqInteger, 
        max_depth: Option<u32>
    ) -> SqGetResult<IndexMap<K, V>> {
        sq_validate!(self.get_type(idx), SqType::Table, SqType::Class)
            .map_err(|e| e.into_stack_error("failed to get table"))?;
        
        if matches!(max_depth, Some(depth) if depth == 0) {
            return Ok(IndexMap::new());
        }

        let mut out = IndexMap::new();

        for pair in self.iter_table(idx, max_depth) {
            match pair {
                Ok((k, v)) => {
                    out.insert(k, v);
                },
                Err(e) => return Err(e),
            } 
        }

        Ok(out)
    }
}

impl<S> SqGet<SqInstance> for Vm<S> where S: VmDrop {
    fn get_constrain(&self, idx: SqInteger, max_depth: Option<u32>) -> SqGetResult<SqInstance> {
        sq_validate!(self.get_type(idx), SqType::Instance)
            .map_err(|e| e.into_stack_error("failed to get instance"))?;
        
        if matches!(max_depth, Some(depth) if depth == 0) {
            return Ok(SqInstance { this: IndexMap::new() });
        }

        // Get instance class table with keys and default values
        self.get_instance_class(idx)
            .map_err(|e| e.into_stack_error("failed to get instance class"))?;
        let proto: DynSqVar = self.get_constrain(-1, Some(1))?;

        let DynSqVar::Class(mut proto) = proto else { unreachable!("not a class") };
        // Pop class
        self.pop(1);

        let next_depth = max_depth.map(|d| d - 1);
        for (key, val) in &mut proto {
            // Push class field/method key and get instance value
            self.push(key.clone()).into_result()?;
            self.slot_get_raw(idx - idx.is_negative() as isize)
                .map_err(|e| e.into_stack_error("failed to get slot of instance"))?;

            *val = self.get_constrain(-1, next_depth)?;

            // Clear stack
            self.pop(1);
        }

        Ok(SqInstance { this: proto })
    }
}

impl<S> SqGet<SqClosureInfo> for Vm<S> where S: VmDrop {
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> SqGetResult<SqClosureInfo> {
        let mut info = SqClosureInfo { name: None, args: vec![], src: None };

        self.get_closure_info(idx)
            .map_err(|e| e.into_stack_error("failed to get closure info"))?;

        // Parameters are 1d array of strings
        for pair in self.iter_table(-1, Some(2)) {
            let (key, val): (String, DynSqVar) = match pair {
                Ok(kv) => kv,
                Err(e) => return Err(e),
            };
            match key.as_str() {
                "name" => if let DynSqVar::String(n) = val {
                    info.name = Some(n);
                },
                "parameters" => if let DynSqVar::Array(v) = val {
                    for arg in v {
                        let DynSqVar::String(arg) = arg else { unreachable!() };
                        info.args.push(arg)
                    }
                }
                "src" => if let DynSqVar::String(n_src) = val {
                    info.src = Some(n_src)
                }
                _ => ()
            }
        }

        // Pop closure info
        self.pop(1);

        Ok(info)
    }
}

impl<S> SqGet<SqNativeClosureInfo> for Vm<S> where S: VmDrop {
    fn get_constrain(&self, idx: SqInteger, _: Option<u32>) -> SqGetResult<SqNativeClosureInfo> {
        let mut info = SqNativeClosureInfo { name: None, arg_types: vec![] };

        self.get_closure_info(idx)
            .map_err(|e| e.into_stack_error("failed to get native closure info"))?;

        // Argument types are 1d array of integers
        for pair in self.iter_table(-1, Some(2)) {
            let (key, val): (String, DynSqVar) = match pair {
                Ok(kv) => kv,
                Err(e) => return Err(e),
            };
            match key.as_str() {
                "name" => if let DynSqVar::String(n) = val {
                    info.name = Some(n);
                },
                // Fill vector with Any to represent untyped argument
                "paramscheck" => if let DynSqVar::Integer(nparams) = val {
                    for _ in 0..nparams {
                        info.arg_types.push(SqTypedArgMask::Any);
                    }
                }
                "typecheck" => if let DynSqVar::Array(v) = val {
                    for (idx, mask) in v.into_iter().enumerate() {
                        let DynSqVar::Integer(mask) = mask else { unreachable!() };
                        let mask = SqTypedArgMask::from_bits_truncate(mask as u32);
                        if idx >= info.arg_types.len() {
                            info.arg_types.push(mask)
                        } else {
                            info.arg_types[idx] = mask; 
                        }
                    }
                }
                _ => ()
            }
        }

        // Pop closure info
        self.pop(1);
        
        Ok(info)
    }
}

impl<S> SqGet<DynSqVar> for Vm<S> where S: VmDrop {
    fn get_constrain(&self, idx: SqInteger, max_depth: Option<u32>) -> SqGetResult<DynSqVar> {
        let sq_type = self.get_type(idx);

        // If container, do not expand
        if matches!(max_depth, Some(depth) 
            if depth == 0 && sq_type.is_complex() && !sq_type.is_closure()
        ) {
            return Ok(DynSqVar::NotExpanded(sq_type))
        }

        // Otherwise just pass max_depth without changes
        match self.get_type(idx) {
            SqType::Null => Ok(DynSqVar::Null),
            SqType::Integer => Ok(DynSqVar::Integer(self.get_constrain(idx, max_depth)?)),
            SqType::String => Ok(DynSqVar::String(self.get_constrain(idx, max_depth)?)),
            SqType::Table => Ok(DynSqVar::Table(self.get_constrain(idx, max_depth)?)),
            SqType::Class => Ok(DynSqVar::Class(self.get_constrain(idx, max_depth)?)),
            SqType::Instance => Ok(DynSqVar::Instance(self.get_constrain(idx, max_depth)?)),
            SqType::Array => Ok(DynSqVar::Array(self.get_constrain(idx, max_depth)?)),
            SqType::Float => Ok(DynSqVar::Float(self.get_constrain(idx, max_depth)?)),
            SqType::Bool => Ok(DynSqVar::Bool(self.get_constrain(idx, max_depth)?)),
            SqType::UserData => Ok(DynSqVar::UserData(self.get_constrain(idx, max_depth)?)),
            SqType::UserPointer => Ok(DynSqVar::UserPointer(self.get_constrain(idx, max_depth)?)),
            SqType::Closure => Ok(DynSqVar::Closure(self.get_constrain(idx, max_depth)?)),
            SqType::NativeClosure => Ok(DynSqVar::NativeClosure(self.get_constrain(idx, max_depth)?)),
            other => Ok(DynSqVar::NotExpanded(other)),
        }
    }
}

