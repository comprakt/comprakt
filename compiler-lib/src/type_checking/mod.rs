pub mod builtin_types;
pub mod checker;
pub mod expr_typechecker;
pub mod method_body_typechecker;
pub mod type_analysis;
pub mod type_system;

pub use self::checker::check;
