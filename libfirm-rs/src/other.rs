use libfirm_rs_bindings as bindings;
use super::nodes_gen::{Block, Start, End};
use std::ffi::{CString};

#[derive(Clone, Copy)]
pub struct Graph {
    pub(super) irg: *mut bindings::ir_graph,
}

impl Graph {
    pub fn entity(self) -> *mut bindings::ir_entity {
        unsafe { bindings::get_irg_entity(self.irg) }
    }

    pub fn start_block(self) -> Block {
        Block::new(unsafe { bindings::get_irg_start_block(self.irg) })
    }

    pub fn end_block(self) -> Block {
        Block::new(unsafe { bindings::get_irg_end_block(self.irg) })
    }

    pub fn start(self) -> Start {
		Start::new(unsafe { bindings::get_irg_start(self.irg) })
	}

    pub fn end(self) -> End {
		End::new(unsafe { bindings::get_irg_end(self.irg) })
	}

    pub fn dump(self, suffix: &str) {
        let suffix = CString::new(suffix).unwrap();
        unsafe { bindings::dump_ir_graph(self.irg, suffix.as_ptr()) }
    }
}

