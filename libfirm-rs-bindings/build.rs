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

    let bindings = bindgen::Builder::default()
        .header("wrapper.h")
        .clang_args(&[&format!("-I{}", libfirm_include_dir.display())])
        // bindgen's own tests fail on that, see
        // https://github.com/rust-lang-nursery/rust-bindgen/issues/550#issuecomment-283438998
        .blacklist_type("max_align_t")
        // comments include code examples which don't work without using self
        .generate_comments(false)
        .generate()
        .expect("Failed to generate bindings");

    let out_path = cargo_envpath("OUT_DIR").join("bindings.rs");
    bindings
        .write_to_file(&out_path)
        .expect(&format!("Failed to write bindings to {:?}", out_path));
}
