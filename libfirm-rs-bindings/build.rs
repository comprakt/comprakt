//! Generate libfirm bindings
use std::{
    env,
    path::PathBuf,
    process::{exit, Command},
};

macro_rules! tell_cargo {
    ($opt: expr, $val: expr) => {
        println!("cargo:{}={}", $opt, $val)
    };
}

fn cargo_envpath(varname: &str) -> PathBuf {
    let r: String = env::var(varname).expect(&format!("env variable {} not set", varname));
    PathBuf::from(r)
}

use std::fs;

#[derive(Debug, Clone, Copy)]
enum ConstifiedEnumRewrite {
    /// enum_name is something like `Some("enum ir_relation")`
    /// variant is something like `ir_relation_.*`
    WithEnumPrefix(&'static str, Option<&'static str>),
    /// enum_name is something like `Some("ir_relation")`
    /// variant is something like `ir_relation_.*`
    Solo(&'static str, Option<&'static str>),
}

impl ConstifiedEnumRewrite {
    fn bindgen_module_name(&self) -> String {
        match self {
            ConstifiedEnumRewrite::WithEnumPrefix(n, _) => format!("{}", n),
            ConstifiedEnumRewrite::Solo(n, _) => format!("{}", n),
        }
    }
    fn bindgen_enum_name(&self) -> String {
        match self {
            ConstifiedEnumRewrite::WithEnumPrefix(n, _) => format!("enum {}", n),
            ConstifiedEnumRewrite::Solo(n, _) => format!("{}", n),
        }
    }
    fn trim_start_matches_prefix(&self) -> String {
        match self {
            ConstifiedEnumRewrite::WithEnumPrefix(_, Some(strip_prefix)) => {
                format!("{}", strip_prefix)
            }
            ConstifiedEnumRewrite::WithEnumPrefix(n, None) => format!("{}_", n),
            ConstifiedEnumRewrite::Solo(n, None) => format!("{}_", n),
            ConstifiedEnumRewrite::Solo(_, Some(strip_prefix)) => format!("{}", strip_prefix),
        }
    }
}

use std::collections::HashMap;

#[derive(Debug, Clone)]
struct EnumRewriter {
    rules: Vec<ConstifiedEnumRewrite>,
    rewrites: HashMap<String, String>, // bindgen_enum_name => trim_start_matches prefix
}

impl EnumRewriter {
    fn from_rewrites(rules: Vec<ConstifiedEnumRewrite>) -> Self {
        let mut rewrites = HashMap::new();
        for rule in &rules {
            let res = rewrites.insert(rule.bindgen_enum_name(), rule.trim_start_matches_prefix());
            debug_assert!(
                res.is_none(),
                "duplicate in constified_enum_rewrites: {:?}",
                rule
            );
        }
        return EnumRewriter { rewrites, rules };
    }
    fn builder_with_rewrites(&self, builder: bindgen::Builder) -> bindgen::Builder {
        let mut builder = builder;
        for rule in &self.rules {
            builder = builder.constified_enum_module(rule.bindgen_module_name());
        }
        return builder;
    }
}

use bindgen::callbacks::*;
use inflections::case;

impl bindgen::callbacks::ParseCallbacks for EnumRewriter {
    fn enum_variant_name(
        &self,
        enum_name: Option<&str>,
        original_variant_name: &str,
        variant_value: EnumVariantValue,
    ) -> Option<String> {
        if let Some(trim_prefix) = enum_name.and_then(|n| self.rewrites.get(n)) {
            let trimmed = original_variant_name.trim_start_matches(trim_prefix);
            let rewritten = case::to_pascal_case(trimmed);
            println!(
                "rewriting {:?} {:?} => {:?}",
                rewritten, original_variant_name, rewritten,
            );
            return Some(rewritten);
        }

        println!(
            "unhandled: {:?} {:?} {:?}",
            enum_name, original_variant_name, variant_value
        );
        return None;
    }
}

fn main() {
    let libfirm_src = cargo_envpath("CARGO_MANIFEST_DIR").join("libfirm");
    let libfirm_src = fs::canonicalize(&libfirm_src).expect(&format!(
        "cannot canonicalize path to libfirm src = {:?}",
        libfirm_src
    ));

    let libfirm_install_dir = cargo_envpath("OUT_DIR").join("libfirm_install");
    assert!(libfirm_install_dir.is_absolute());
    fs::create_dir_all(&libfirm_install_dir).expect(&format!(
        "cannot create libfirm install directory {:?}",
        libfirm_install_dir
    ));

    println!("libfirm_src={:?}", libfirm_src);
    println!("libfirm_install_dir={:?}", libfirm_install_dir);

    let exit_code = Command::new("make")
        .current_dir(libfirm_src.as_path())
        .arg("install")
        .arg(format!(
            "-j{}",
            env::var("NUM_JOBS").expect("env var NUM_JOBS not set")
        ))
        .arg(format!("PREFIX={}", libfirm_install_dir.display()))
        .status()
        .expect("Failed to run make")
        .code()
        .unwrap_or(1);

    if exit_code != 0 {
        eprintln!("Failed to build libfirm");
        exit(exit_code);
    }

    let libfirm_include_dir = libfirm_install_dir.join("include");
    let libfirm_include_dir_inner = libfirm_include_dir.join("libfirm");
    let libfirm_ir_dir = libfirm_src.join("ir");
    let libfirm_adt_dir = libfirm_include_dir_inner.join("adt");
    let libfirm_lib_dir = libfirm_install_dir.join("lib");
    tell_cargo!("include", libfirm_include_dir.display());
    tell_cargo!("include", libfirm_include_dir_inner.display());
    tell_cargo!("include", libfirm_adt_dir.display());
    tell_cargo!("include", libfirm_ir_dir.display());
    tell_cargo!("rustc-link-search", libfirm_lib_dir.display());
    tell_cargo!("rustc-link-lib=static", "firm");

    let builder = bindgen::Builder::default()
        .header("wrapper.h")
        .clang_args(&[&format!("-I{}", libfirm_include_dir.display())])
        .clang_args(&[&format!("-I{}", libfirm_include_dir_inner.display())])
        .clang_args(&[&format!("-I{}", libfirm_adt_dir.display())])
        .clang_args(&[&format!("-I{}", libfirm_ir_dir.display())])
        // bindgen's own tests fail on that, see
        // https://github.com/rust-lang-nursery/rust-bindgen/issues/550#issuecomment-283438998
        .blacklist_type("max_align_t")
        // comments include code examples which don't work without using self
        .generate_comments(false);

    use self::ConstifiedEnumRewrite::*;
    #[rustfmt::skip]
    let enum_rewriter = EnumRewriter::from_rewrites(vec![

        // Bulk edited
        WithEnumPrefix("op_pin_state", None),
        WithEnumPrefix("cond_jmp_predicate", Some("COND_JMP_PRED_")),
        WithEnumPrefix("ir_builtin_kind", Some("ir_bk_")),
        WithEnumPrefix("float_int_conversion_overflow_style_t", Some("ir_overflow_")),
        WithEnumPrefix("ir_linkage", Some("IR_LINKAGE_")),
        WithEnumPrefix("ir_initializer_kind_t", Some("IR_INITIALIZER_")),
        WithEnumPrefix("ptr_access_kind", Some("ptr_access_")),
        WithEnumPrefix("tp_opcode", Some("tpo_")),
        WithEnumPrefix("__codecvt_result", Some("__codecvt_")),
        WithEnumPrefix("osr_flags", Some("osr_flag_")),
        WithEnumPrefix("ir_mode_arithmetic", Some("irma_")),
        WithEnumPrefix("asm_constraint_flags_t", Some("ASM_CONSTRAINT_FLAG_")),
        WithEnumPrefix("firm_kind", Some("k_")),
        WithEnumPrefix("ir_opcode", Some("iro_")),
        WithEnumPrefix("ir_edge_kind_t", Some("EDGE_KIND_")),
        WithEnumPrefix("ir_relation", None),
        WithEnumPrefix("ir_resources_t", Some("IR_RESOURCE_")),
        WithEnumPrefix("ir_graph_constraints_t", Some("IR_GRAPH_CONSTRAINT_")),
        WithEnumPrefix("ir_graph_properties_t", Some("IR_GRAPH_PROPERTY_")),
        WithEnumPrefix("ir_alias_relation", Some("ir_")),
        WithEnumPrefix("ir_entity_usage_computed_state", Some("ir_entity_usage_")),
        WithEnumPrefix("ir_disambiguator_options", Some("aa_opt_")),
        WithEnumPrefix("ir_segment_t", Some("IR_SEGMENT_")),
        WithEnumPrefix("irp_resources_t", Some("IRP_RESOURCE_")),
        WithEnumPrefix("ikind", Some("INTRINSIC_")),
        WithEnumPrefix("ir_platform_type_t", Some("IR_TYPE_")),
        WithEnumPrefix("range_types", Some("VRP_")),
        WithEnumPrefix("mtp_additional_properties", Some("mtp_")),
        WithEnumPrefix("ir_cons_flags", Some("cons_")),

        Solo("ir_volatility", Some("volatility_")),
        Solo("ir_align", Some("align_")),
        Solo("ir_visibility", None),
        Solo("ir_entity_usage", Some("ir_usage_")),
        Solo("inh_transitive_closure_state", Some("inh_transitive_closure_")),
        Solo("ir_type_state", Some("layout_")),
        Solo("calling_convention", Some("cc_")),
        Solo("dwarf_source_language", Some("DW_LANG_")),
        Solo("irp_callgraph_state", Some("irp_callgraph_")),
        Solo("loop_nesting_depth_state", Some("loop_nesting_depth_")),
        Solo("dbg_action", Some("dbg_")),
        Solo("op_arity", Some("oparity_")),
        Solo("irop_flags", Some("irop_flag_")),
        Solo("dump_reason_t", Some("dump_node_")),
        Solo("n_ASM", None),
        Solo("pn_ASM", None),
        Solo("n_Add", None),
        Solo("n_Alloc", None),
        Solo("pn_Alloc", None),
        Solo("n_Anchor", None),
        Solo("n_And", None),
        Solo("n_Bitcast", None),
        Solo("n_Builtin", None),
        Solo("pn_Builtin", None),
        Solo("n_Call", None),
        Solo("pn_Call", None),
        Solo("n_Cmp", None),
        Solo("n_Cond", None),
        Solo("n_Confirm", None),
        Solo("n_Conv", None),
        Solo("n_CopyB", None),
        Solo("n_Div", None),
        Solo("pn_Div", None),
        Solo("n_Eor", None),
        Solo("n_Free", None),
        Solo("n_IJmp", None),
        Solo("n_Id", None),
        Solo("n_Load", None),
        Solo("n_Member", None),
        Solo("n_Minus", None),
        Solo("n_Mod", None),
        Solo("pn_Mod", None),
        Solo("n_Mul", None),
        Solo("n_Mulh", None),
        Solo("n_Mux", None),
        Solo("n_Not", None),
        Solo("n_Or", None),
        Solo("n_Pin", None),
        Solo("n_Proj", None),
        Solo("n_Raise", None),
        Solo("pn_Raise", None),
        Solo("n_Return", None),
        Solo("n_Sel", None),
        Solo("n_Shl", None),
        Solo("n_Shr", None),
        Solo("n_Shrs", None),
        Solo("pn_Start", None),
        Solo("n_Store", None),
        Solo("n_Sub", None),
        Solo("n_Switch", None),
        Solo("pn_Switch", None),
        Solo("ir_dump_verbosity_t", Some("dump_verbosity_")),
        Solo("ir_dump_flags_t", Some("ir_dump_flag_")),
        Solo("irg_callee_info_state", Some("irg_callee_info_")),
        Solo("pn_Cond", None),
        Solo("pn_Load", None),
        Solo("pn_Store", None),
    ]);

    let builder = builder.parse_callbacks(Box::new(enum_rewriter.clone()));
    let builder = enum_rewriter.builder_with_rewrites(builder);

    let bindings = builder.generate().expect("Failed to generate bindings");

    let out_path = cargo_envpath("OUT_DIR").join("bindings.rs");
    bindings
        .write_to_file(&out_path)
        .expect(&format!("Failed to write bindings to {:?}", out_path));
}
