use std::path::PathBuf;

use std::process::Command;

fn main() {
    let this_crate_dir = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let impl_crate_dir = this_crate_dir.ancestors().nth(1).unwrap().join("mjrt-impl");

    assert!(
        std::fs::read_to_string(impl_crate_dir.clone().join("Cargo.toml"))
            .unwrap()
            .contains("mjrt-impl")
    );

    println!(
        "invoking cargo build in mjrt-impl crate at {:?}",
        impl_crate_dir
    );

    Command::new(env!("CARGO"))
        .current_dir(impl_crate_dir.clone())
        .arg("build")
        .status()
        .expect("could not build mjrt/src");

    let static_lib_path = impl_crate_dir
        .join("target")
        .join("debug")
        .join("libmjrt_impl.a");
    debug_assert!(
        static_lib_path.exists(),
        "static lib artifact was not built, expected at {:?}",
        static_lib_path
    );

    println!(
        "cargo:rustc-env=MJRT_STATIC_LIB_PATH={}",
        static_lib_path.display()
    )
}
