use libfirm_rs::{entity::Entity, types::*};

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
            Entity::new_global(&"mjrt_dumpstack", t.into())
        };

        let system_out_println = {
            let it = PrimitiveTy::i32();
            let mut t = MethodTyBuilder::new();
            t.add_param(it.into());
            let t = t.build_no_this_call();
            Entity::new_global(&"mjrt_system_out_println", t.into())
        };

        let system_out_write = {
            let it = PrimitiveTy::i32();
            let mut t = MethodTyBuilder::new();
            t.add_param(it.into());
            let t = t.build_no_this_call();
            Entity::new_global(&"mjrt_system_out_write", t.into())
        };

        let system_out_flush = {
            let t = MethodTyBuilder::new().build_no_this_call();
            Entity::new_global(&"mjrt_system_out_flush", t.into())
        };

        let system_in_read = {
            let it = PrimitiveTy::i32();
            let mut t = MethodTyBuilder::new();
            t.set_res(it.into());
            let t = t.build_no_this_call();
            Entity::new_global(&"mjrt_system_in_read", t.into())
        };

        let new = {
            let loc = PrimitiveTy::ptr();
            let size = PrimitiveTy::i32();
            let mut t = MethodTyBuilder::new();

            t.add_param(size.into());
            t.set_res(loc.into());

            let t = t.build_no_this_call();
            Entity::new_global(&"mjrt_new", t.into())
        };

        let div_by_zero = {
            let t = MethodTyBuilder::new().build_no_this_call();
            Entity::new_global(&"mjrt_div_by_zero", t.into())
        };

        let null_usage = {
            let t = MethodTyBuilder::new().build_no_this_call();
            Entity::new_global(&"mjrt_null_usage", t.into())
        };

        let array_out_of_bounds = {
            let t = MethodTyBuilder::new().build_no_this_call();
            Entity::new_global(&"mjrt_array_out_of_bounds", t.into())
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
