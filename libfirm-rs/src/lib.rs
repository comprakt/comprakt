#![allow(clippy::not_unsafe_ptr_arg_deref)]
#![warn(clippy::print_stdout)]
#![feature(range_contains)]
#![feature(uniform_paths)]

#[macro_use]
extern crate derive_more;
pub use libfirm_rs_bindings as bindings;

mod entity;
mod graph;
mod mode;
pub mod nodes;
mod tarval;
pub mod types;

pub use self::{
    entity::Entity,
    graph::{Graph, VisitTime},
    mode::Mode,
    tarval::{Tarval, TarvalKind},
};

// uncomment for racer support:
// extern crate libfirm_rs_bindings;

use std::sync::Once;

static INIT: Once = Once::new();
pub fn init() {
    INIT.call_once(|| unsafe {
        bindings::ir_init_library();
    });
}
