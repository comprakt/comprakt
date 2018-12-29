#![allow(clippy::not_unsafe_ptr_arg_deref)]
#![warn(clippy::print_stdout)]
#![feature(range_contains)]
pub use libfirm_rs_bindings as bindings;
#[macro_use]
extern crate derive_more;

// extern crate libfirm_rs_bindings;

pub mod entity;

pub mod nodes;

pub mod graph;
pub use self::graph::Graph;
#[allow(dead_code)]
pub mod nodes_gen;
pub mod types;

pub mod tarval;
pub mod value_nodes;

mod mode;
pub use self::mode::Mode;

use std::sync::Once;

static INIT: Once = Once::new();
pub fn init() {
    INIT.call_once(|| unsafe {
        bindings::ir_init_library();
    });
}
