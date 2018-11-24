//! This module implements type checking on the AST. The type checking
//! produces an instance of `type_system::TypeSystem`, which is later used by
//! the `firm` module for IR graph construction.
//!
//! ## `$` Types (`builtin_types`)
//!
//! MiniJava suffers from the fact that its specification was stripped down
//! from the Java language report.  A good example is the handling of the main
//! method's arguments: `String[] args` must not be used, and according to the
//! spec, `String[]` within the main method's parameter list is not a type.
//! However, it is possible for users to define a `class String` in MiniJava,
//! which ,in Java, would shadow the `java.lang.String` that is part of the
//! main method's parameter type. Not so in MiniJava - at least that's the
//! result of the discussion we had in the lecture.
//!
//! To accomodate this and other spec quirks, we define built-in types using
//! `$String` or similar: Since a class definition beginning with `$` is
//! rejected by the lexer, we can safely introduce such definitions during type
//! analysis for the built-in types.

pub mod builtin_types;
pub mod checker;
pub mod method_body_type_checker;
pub mod type_analysis;
pub mod type_system;

pub use self::checker::check;
