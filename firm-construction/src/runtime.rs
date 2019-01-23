use libfirm_rs::{types::*, Entity};
use strum_macros::EnumDiscriminants;

use crate::type_checking::type_system;

#[strum_discriminants(derive(Display))]
#[derive(EnumDiscriminants)]
pub enum RuntimeFunction {
    SystemOutPrintln,
    SystemOutWrite,
    SystemOutFlush,
    SystemInRead,
    New,
    Dumpstack,
    NullUsage,
    ArrayOutOfBounds,
    DivByZero,
}

pub trait RTLib {
    fn ld_name(&self, builtin: RuntimeFunction) -> &'static str;
    fn mj_main_name(&self) -> &'static str;
}

impl From<type_system::BuiltinMethodBody> for RuntimeFunction {
    fn from(mb: type_system::BuiltinMethodBody) -> RuntimeFunction {
        use self::type_system::BuiltinMethodBody;
        match mb {
            BuiltinMethodBody::SystemOutPrintln => RuntimeFunction::SystemOutPrintln,
            BuiltinMethodBody::SystemOutWrite => RuntimeFunction::SystemOutWrite,
            BuiltinMethodBody::SystemOutFlush => RuntimeFunction::SystemOutFlush,
            BuiltinMethodBody::SystemInRead => RuntimeFunction::SystemInRead,
        }
    }
}

/// The runtime library implemented by this compiler in crate mjrt-impl.
pub struct Mjrt;

impl RTLib for Mjrt {
    fn ld_name(&self, rtf: RuntimeFunction) -> &'static str {
        match rtf {
            RuntimeFunction::SystemOutPrintln => "mjrt_system_out_println",
            RuntimeFunction::SystemOutWrite => "mjrt_system_out_write",
            RuntimeFunction::SystemOutFlush => "mjrt_system_out_flush",
            RuntimeFunction::SystemInRead => "mjrt_system_in_read",
            RuntimeFunction::Dumpstack => "mjrt_dumpstack",
            RuntimeFunction::DivByZero => "mjrt_div_by_zero",
            RuntimeFunction::NullUsage => "mjrt_null_usage",
            RuntimeFunction::ArrayOutOfBounds => "mjrt_array_out_of_bounds",
            RuntimeFunction::New => "mjrt_new",
        }
    }
    fn mj_main_name(&self) -> &'static str {
        "mj_main"
    }
}

/// The runtime library provided implemented by molki.
pub struct Molki;

impl RTLib for Molki {
    fn ld_name(&self, rtf: RuntimeFunction) -> &'static str {
        match rtf {
            RuntimeFunction::SystemOutPrintln => "__stdlib_println",
            RuntimeFunction::SystemOutWrite => "__stdlib_write",
            RuntimeFunction::SystemOutFlush => "__stdlib_flush",
            RuntimeFunction::SystemInRead => "__stdlib_read",
            RuntimeFunction::New => "__stdlib_malloc",
            RuntimeFunction::Dumpstack => "__stdlib_not_implemented_dumpstack",
            RuntimeFunction::DivByZero => "__stdlib_not_implemented_div_by_zero",
            RuntimeFunction::NullUsage => "__stdlib_not_implemented_null_usage",
            RuntimeFunction::ArrayOutOfBounds => "__stdlib_not_implemented_array_oob",
        }
    }
    fn mj_main_name(&self) -> &'static str {
        "minijava_main"
    }
}

pub struct Runtime {
    pub lib: Box<dyn RTLib>,

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

impl Runtime {
    pub fn new(lib: Box<dyn RTLib>) -> Runtime {
        let dumpstack = {
            let t = MethodTyBuilder::new().build_no_this_call();
            Entity::new_global(lib.ld_name(RuntimeFunction::Dumpstack), t.into())
        };

        let system_out_println = {
            let it = PrimitiveTy::i32();
            let mut t = MethodTyBuilder::new();
            t.add_param(it.into());
            let t = t.build_no_this_call();
            Entity::new_global(lib.ld_name(RuntimeFunction::SystemOutPrintln), t.into())
        };

        let system_out_write = {
            let it = PrimitiveTy::i32();
            let mut t = MethodTyBuilder::new();
            t.add_param(it.into());
            let t = t.build_no_this_call();
            Entity::new_global(lib.ld_name(RuntimeFunction::SystemOutWrite), t.into())
        };

        let system_out_flush = {
            let t = MethodTyBuilder::new().build_no_this_call();
            Entity::new_global(lib.ld_name(RuntimeFunction::SystemOutFlush), t.into())
        };

        let system_in_read = {
            let it = PrimitiveTy::i32();
            let mut t = MethodTyBuilder::new();
            t.set_res(it.into());
            let t = t.build_no_this_call();
            Entity::new_global(lib.ld_name(RuntimeFunction::SystemInRead), t.into())
        };

        let new = {
            let loc = PrimitiveTy::ptr();
            let size = PrimitiveTy::i32();
            let mut t = MethodTyBuilder::new();

            t.add_param(size.into());
            t.set_res(loc.into());

            let t = t.build_no_this_call();
            Entity::new_global(lib.ld_name(RuntimeFunction::New), t.into())
        };

        let div_by_zero = {
            let t = MethodTyBuilder::new().build_no_this_call();
            Entity::new_global(lib.ld_name(RuntimeFunction::DivByZero), t.into())
        };

        let null_usage = {
            let t = MethodTyBuilder::new().build_no_this_call();
            Entity::new_global(lib.ld_name(RuntimeFunction::NullUsage), t.into())
        };

        let array_out_of_bounds = {
            let t = MethodTyBuilder::new().build_no_this_call();
            Entity::new_global(lib.ld_name(RuntimeFunction::ArrayOutOfBounds), t.into())
        };

        Runtime {
            lib,
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
