#![allow(clippy::all)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

/// A little more idiomatic access to the mode_* statics.
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
    pub type Type = *mut super::ir_mode;
}

/// A little more idiomatic access to the op_* statics.
pub mod op {
    pub use super::op_ASM as ASM;
    pub use super::op_Add as Add;
    pub use super::op_Address as Address;
    pub use super::op_Align as Align;
    pub use super::op_Alloc as Alloc;
    pub use super::op_Anchor as Anchor;
    pub use super::op_And as And;
    pub use super::op_Bad as Bad;
    pub use super::op_Bitcast as Bitcast;
    pub use super::op_Block as Block;
    pub use super::op_Builtin as Builtin;
    pub use super::op_Call as Call;
    pub use super::op_Cmp as Cmp;
    pub use super::op_Cond as Cond;
    pub use super::op_Confirm as Confirm;
    pub use super::op_Const as Const;
    pub use super::op_Conv as Conv;
    pub use super::op_CopyB as CopyB;
    pub use super::op_Deleted as Deleted;
    pub use super::op_Div as Div;
    pub use super::op_Dummy as Dummy;
    pub use super::op_End as End;
    pub use super::op_Eor as Eor;
    pub use super::op_Free as Free;
    pub use super::op_IJmp as IJmp;
    pub use super::op_Id as Id;
    pub use super::op_Jmp as Jmp;
    pub use super::op_Load as Load;
    pub use super::op_Member as Member;
    pub use super::op_Minus as Minus;
    pub use super::op_Mod as Mod;
    pub use super::op_Mul as Mul;
    pub use super::op_Mulh as Mulh;
    pub use super::op_Mux as Mux;
    pub use super::op_NoMem as NoMem;
    pub use super::op_Not as Not;
    pub use super::op_Offset as Offset;
    pub use super::op_Or as Or;
    pub use super::op_Phi as Phi;
    pub use super::op_Pin as Pin;
    pub use super::op_Proj as Proj;
    pub use super::op_Raise as Raise;
    pub use super::op_Return as Return;
    pub use super::op_Sel as Sel;
    pub use super::op_Shl as Shl;
    pub use super::op_Shr as Shr;
    pub use super::op_Shrs as Shrs;
    pub use super::op_Size as Size;
    pub use super::op_Start as Start;
    pub use super::op_Store as Store;
    pub use super::op_Sub as Sub;
    pub use super::op_Switch as Switch;
    pub use super::op_Sync as Sync;
    pub use super::op_Tuple as Tuple;
    pub use super::op_Unknown as Unknown;
}


