#![allow(clippy::all)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

/// A little more idiomatic access to the mode_* globals
pub mod mode {
    pub use super::mode_ANY as Any;
    pub use super::mode_BAD as BAD;
    pub use super::mode_BB as BB;
    pub use super::mode_Bs as Bs;
    pub use super::mode_Bu as Bu;
    pub use super::mode_D as D;
    pub use super::mode_F as F;
    pub use super::mode_Hs as Hs;
    pub use super::mode_Hu as Hu;
    pub use super::mode_Is as Is;
    pub use super::mode_Iu as Iu;
    pub use super::mode_Ls as Ls;
    pub use super::mode_Lu as Lu;
    pub use super::mode_M as M;
    pub use super::mode_P as P;
    pub use super::mode_T as T;
    pub use super::mode_X as X;
    pub use super::mode_b as b;
}
