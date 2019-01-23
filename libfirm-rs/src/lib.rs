#![allow(clippy::not_unsafe_ptr_arg_deref)]
#![warn(clippy::print_stdout)]
#![feature(range_contains)]

#[macro_use]
extern crate derive_more;

#[macro_use]
extern crate lazy_static;

macro_rules! generate_iterator {
    (
        $iter_name: ident,
        $info_type: ty,
        $len_fn: ident,
        $info_name: ident,
        $idx_name: ident,
        $idx_type: ty,
        $result_expr: expr,
        $result_type: ty,
    ) => {
        pub struct $iter_name {
            info: $info_type,
            cur: $idx_type,
            len: $idx_type,
        }

        impl $iter_name {
            fn new(info: $info_type) -> Self {
                Self {
                    info,
                    len: unsafe { bindings::$len_fn(info) },
                    cur: 0,
                }
            }

            pub fn idx(&self, $idx_name: $idx_type) -> Option<$result_type> {
                if (0..self.len).contains(&$idx_name) {
                    let $info_name = self.info;
                    Some($result_expr)
                } else {
                    None
                }
            }
        }

        impl Iterator for $iter_name {
            type Item = $result_type;

            fn next(&mut self) -> Option<$result_type> {
                if self.cur == self.len {
                    None
                } else {
                    let $idx_name = self.cur;
                    self.cur += 1;
                    let $info_name = self.info;
                    Some($result_expr)
                }
            }
        }

        impl ExactSizeIterator for $iter_name {
            fn len(&self) -> usize {
                self.len as usize
            }
        }
    };
}

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

use std::sync::Once;

static INIT: Once = Once::new();
pub fn init() {
    INIT.call_once(|| unsafe {
        bindings::ir_init_library();
    });
}
