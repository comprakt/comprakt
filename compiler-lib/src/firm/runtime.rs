use crate::type_checking::type_system::BuiltinMethodBody;
use libfirm_rs::{bindings::*, *};
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
            let t = FunctionType::new().build(false);
            let id = CString::new("mjrt_dumpstack").unwrap();
            Entity::new_global(&id, t)
        };

        let system_out_println = {
            let it = PrimitiveType::i32();
            let mut t = FunctionType::new();
            t.add_param(it);
            let t = t.build(false);
            let id = CString::new("mjrt_system_out_println").unwrap();
            Entity::new_global(&id, t)
        };

        let system_out_write = {
            let it = PrimitiveType::i32();
            let mut t = FunctionType::new();
            t.add_param(it);
            let t = t.build(false);
            let id = CString::new("mjrt_system_out_write").unwrap();
            Entity::new_global(&id, t)
        };

        let system_out_flush = {
            let t = FunctionType::new().build(false);
            let id = CString::new("mjrt_system_out_flush").unwrap();
            Entity::new_global(&id, t)
        };

        let system_in_read = {
            let it = PrimitiveType::i32();
            let mut t = FunctionType::new();
            t.set_res(it);
            let t = t.build(false);
            let id = CString::new("mjrt_system_in_read").unwrap();
            Entity::new_global(&id, t)
        };

        let new = {
            let loc = PrimitiveType::ptr();
            let size = PrimitiveType::u32();
            let mut t = FunctionType::new();

            t.add_param(size);
            t.set_res(loc);

            let t = t.build(false);
            let id = CString::new("mjrt_new").unwrap();
            Entity::new_global(&id, t)
        };

        let div_by_zero = {
            let t = FunctionType::new().build(false);
            let id = CString::new("mjrt_div_by_zero").unwrap();
            Entity::new_global(&id, t)
        };

        let null_usage = {
            let t = FunctionType::new().build(false);
            let id = CString::new("mjrt_null_usage").unwrap();
            Entity::new_global(&id, t)
        };

        let array_out_of_bounds = {
            let t = FunctionType::new().build(false);
            let id = CString::new("mjrt_array_out_of_bounds").unwrap();
            Entity::new_global(&id, t)
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

    pub fn graph_from_builtin_method_body(&self, mb: BuiltinMethodBody) -> Graph {
        let (rt_entity, slot_count, has_int_arg, returns_value) = match mb {
            BuiltinMethodBody::SystemOutPrintln => (self.system_out_println, 2, true, false),
            BuiltinMethodBody::SystemOutWrite => (self.system_out_write, 2, true, false),
            BuiltinMethodBody::SystemInRead => (self.system_in_read, 1, false, true),
            BuiltinMethodBody::SystemOutFlush => (self.system_out_flush, 1, false, false),
        };

        // wrap it into a proper function
        let wrapper_function_name = rt_entity.name().to_str().unwrap();
        let wrapper_function_name = format!("$WRAPPER$_{}", wrapper_function_name);

        let graph = Graph::function(&wrapper_function_name, rt_entity.ty(), slot_count);

        let args = if has_int_arg {
            let paramnode = graph.value(1, unsafe { mode::Is }).into();
            vec![paramnode] // slot 1 because 0 is this
        } else {
            vec![]
        };
        let func_addr = graph.new_addr(rt_entity);
        let call = graph
            .cur_block()
            .new_call(graph.cur_store(), func_addr, &args);
        unsafe {
            keep_alive(call.into());
        }
        let call_post_mem = call.project_mem();
        graph.set_store(call_post_mem);

        let mem = graph.cur_store();
        if returns_value {
            let result_tuple = call.project_result_tuple();
            let result_int = result_tuple.project(unsafe { mode::Is }, 0);
            let ret = graph.cur_block().new_return(mem, Some(result_int.into()));
            graph.end_block().add_pred(&ret);
        } else {
            let ret = graph.cur_block().new_return(mem, None);
            graph.end_block().add_pred(&ret);
        }

        graph.cur_block().mature();
        graph.end_block().mature();

        unsafe {
            irg_finalize_cons(graph.into());
        }

        log::debug!("\tgraph done");

        graph
    }
}
