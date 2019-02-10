//! Checks for regresssions in the CLI interface code
//!
//! To skip unit tests, and only run integration tests, execute:
//!
//! ```sh
//! cargo test --test integration
//! ```
//!
//! By default, the main binary is used. But you can specify another
//! binary by setting the `COMPILER_BINARY` environment flag. For
//! example:
//!
//! ```sh
//! COMPILER_BINARY="./run" cargo test --test integration
//! ```
use ::optimization::Level;
use integration_test_codegen::*;
use runner_integration_tests::*;
use std::path::PathBuf;

gen_lexer_integration_tests!();
gen_parser_integration_tests!();
gen_ast_reference_integration_tests!();
gen_ast_idempotence_integration_tests!();
gen_semantic_integration_tests!();
gen_lints_integration_tests!();
gen_ast_inspector_tests!();
gen_assembly_integration_tests!();
gen_binary_integration_tests!();
gen_timeout_integration_tests!();
gen_optimization_integration_tests!();
