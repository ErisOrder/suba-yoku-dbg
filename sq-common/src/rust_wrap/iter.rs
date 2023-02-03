
use std::marker::PhantomData;

use super::vm::{Vm, safety::VmDrop};
use super::get::{SqGet, SqGetResult};

/// Iterator on squirrel array
pub struct SqArrayIter<'vm, S, T> where S: VmDrop {
    pub(super) vm: &'vm Vm<S>,
    pub(super) max_depth: Option<u32>,
    pub(super) _type: PhantomData<T>,
}

impl<S, T> Drop for SqArrayIter<'_, S, T> where S: VmDrop {
    fn drop(&mut self) {
        // Pop the null iterator and the reference
        self.vm.pop(2);
    }
}

impl<S, T> Iterator for SqArrayIter<'_, S, T> 
where 
    S: VmDrop,
    Vm<S>: SqGet<T>
{
    type Item = SqGetResult<T>;

    fn next(&mut self) -> Option<Self::Item> {
        let next_depth = self.max_depth.map(|d| d - 1);
        self.vm.sq_iter_next(-3).map(|_| {
            let elem = self.vm.get_constrain(-1, next_depth);
            // Pop key-val
            if elem.is_ok() {
                self.vm.pop(2);
            }

            elem
        })
    }
}

/// Iterator on squirrel table
pub struct SqTableIter<'vm, S, K, V> where S: VmDrop {
    pub(super) vm: &'vm Vm<S>,
    pub(super) max_depth: Option<u32>,
    pub(super) _type: PhantomData<(K, V)>,
}

impl<S, K, V> Drop for SqTableIter<'_, S, K, V> where S: VmDrop {
    fn drop(&mut self) {
        // Pop the null iterator and the reference
        self.vm.pop(2);
    }
}

impl<S, K, V> Iterator for SqTableIter<'_, S, K, V>
where 
    S: VmDrop,
    Vm<S>: SqGet<K> + SqGet<V>
{
    type Item = SqGetResult<(K, V)>;

    fn next(&mut self) -> Option<Self::Item> {
        let next_depth = self.max_depth.map(|d| d - 1);
        self.vm.sq_iter_next(-3).map(|_| {
            let val: V = match self.vm.get_constrain(-1, next_depth) {
                Ok(v) => v,
                Err(e) => return Err(e),
            };

            let key: K = match self.vm.get_constrain(-2, next_depth) {
                Ok(k) => k,
                Err(e) => return Err(e),
            };

            // Pop key-val
            self.vm.pop(2);
            Ok((key, val))
        })
    }
}
