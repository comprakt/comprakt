use libfirm_rs::{entity::Entity, types::*};
use std::ffi::CString;

pub struct Runtime {
    pub system_out_println: Entity,
    pub system_out_write: Entity,
    pub system_out_flush: Entity,
    pub system_in_read: Entity,
    pub new: Entity,

    pub dumpstack: Entity,
    pub null_usage: Entity,
    pub array_out_of_bounds: Entity,
    pub div_by_zero: Entity,
}

impl Default for Runtime {
    fn default() -> Self {
        Runtime::new()
    }
}

impl Runtime {
    pub fn new() -> Runtime {
        let dumpstack = {
            let t = MethodTyBuilder::new().build_no_this_call();
            let id = CString::new("mjrt_dumpstack").unwrap();
            Entity::new_global(&id, t.into())
        };

        let system_out_println = {
            let it = PrimitiveTy::i32();
            let mut t = MethodTyBuilder::new();
            t.add_param(it.into());
            let t = t.build_no_this_call();
            let id = CString::new("mjrt_system_out_println").unwrap();
            Entity::new_global(&id, t.into())
        };

        let system_out_write = {
            let it = PrimitiveTy::i32();
            let mut t = MethodTyBuilder::new();
            t.add_param(it.into());
            let t = t.build_no_this_call();
            let id = CString::new("mjrt_system_out_write").unwrap();
            Entity::new_global(&id, t.into())
        };

        let system_out_flush = {
            let t = MethodTyBuilder::new().build_no_this_call();
            let id = CString::new("mjrt_system_out_flush").unwrap();
            Entity::new_global(&id, t.into())
        };

        let system_in_read = {
            let it = PrimitiveTy::i32();
            let mut t = MethodTyBuilder::new();
            t.set_res(it.into());
            let t = t.build_no_this_call();
            let id = CString::new("mjrt_system_in_read").unwrap();
            Entity::new_global(&id, t.into())
        };

        let new = {
            let loc = PrimitiveTy::ptr();
            let size = PrimitiveTy::i32();
            let mut t = MethodTyBuilder::new();

            t.add_param(size.into());
            t.set_res(loc.into());

            let t = t.build_no_this_call();
            let id = CString::new("mjrt_new").unwrap();
            Entity::new_global(&id, t.into())
        };

        let div_by_zero = {
            let t = MethodTyBuilder::new().build_no_this_call();
            let id = CString::new("mjrt_div_by_zero").unwrap();
            Entity::new_global(&id, t.into())
        };

        let null_usage = {
            let t = MethodTyBuilder::new().build_no_this_call();
            let id = CString::new("mjrt_null_usage").unwrap();
            Entity::new_global(&id, t.into())
        };

        let array_out_of_bounds = {
            let t = MethodTyBuilder::new().build_no_this_call();
            let id = CString::new("mjrt_array_out_of_bounds").unwrap();
            Entity::new_global(&id, t.into())
        };

        Runtime {
            system_out_println,
            system_out_write,
            system_out_flush,
            system_in_read,
            new,
            dumpstack,
            div_by_zero,
            null_usage,
            array_out_of_bounds,
        }
    }
}
