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
    Solo(&'static str),
}

impl ConstifiedEnumRewrite {
    fn bindgen_module_name(&self) -> String {
        match self {
            ConstifiedEnumRewrite::WithEnumPrefix(n, _) => format!("{}", n),
            ConstifiedEnumRewrite::Solo(n) => format!("{}", n),
        }
    }
    fn bindgen_enum_name(&self) -> String {
        match self {
            ConstifiedEnumRewrite::WithEnumPrefix(n, _) => format!("enum {}", n),
            ConstifiedEnumRewrite::Solo(n) => format!("{}", n),
        }
    }
    fn trim_start_matches_prefix(&self) -> String {
        match self {
            ConstifiedEnumRewrite::WithEnumPrefix(_, Some(strip_prefix)) => {
                format!("{}", strip_prefix)
            }
            ConstifiedEnumRewrite::WithEnumPrefix(n, None) => format!("{}_", n),
            ConstifiedEnumRewrite::Solo(n) => format!("{}_", n),
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
    let libfirm_lib_dir = libfirm_install_dir.join("lib");
    tell_cargo!("include", libfirm_include_dir.display());
    tell_cargo!("rustc-link-search", libfirm_lib_dir.display());
    tell_cargo!("rustc-link-lib=static", "firm");

    let builder = bindgen::Builder::default()
        .header("wrapper.h")
        .clang_args(&[&format!("-I{}", libfirm_include_dir.display())])
        // bindgen's own tests fail on that, see
        // https://github.com/rust-lang-nursery/rust-bindgen/issues/550#issuecomment-283438998
        .blacklist_type("max_align_t")
        // comments include code examples which don't work without using self
        .generate_comments(false);

    let enum_rewriter = EnumRewriter::from_rewrites(vec![
        ConstifiedEnumRewrite::WithEnumPrefix("mtp_additional_properties", Some("mtp_")),
        ConstifiedEnumRewrite::WithEnumPrefix("ir_relation", None),
        ConstifiedEnumRewrite::Solo("pn_Cond"),
        ConstifiedEnumRewrite::Solo("pn_Load"),
        ConstifiedEnumRewrite::Solo("pn_Store"),
        ConstifiedEnumRewrite::WithEnumPrefix("ir_cons_flags", Some("cons_")),
    ]);

    let builder = builder.parse_callbacks(Box::new(enum_rewriter.clone()));
    let builder = enum_rewriter.builder_with_rewrites(builder);

    let bindings = builder.generate().expect("Failed to generate bindings");

    let out_path = cargo_envpath("OUT_DIR").join("bindings.rs");
    bindings
        .write_to_file(&out_path)
        .expect(&format!("Failed to write bindings to {:?}", out_path));
}
