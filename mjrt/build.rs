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

    // approximate inputs (.rs files) of mjrt_impl crate
    // TODO research whether we can get this from cargo metadata
    for dep in walkdir::WalkDir::new(&impl_crate_dir) {
        let p = dep.unwrap();
        match p.path().extension().map(|osstr| osstr.to_str().unwrap()) {
            Some("rs") | Some("toml") | Some("lock") => {
                println!("cargo:rerun-if-changed={}", p.path().display());
            }
            _ => continue,
        }
    }
    println!(
        "invoking cargo build in mjrt-impl crate at {:?}",
        impl_crate_dir
    );


    println!("cargo:rerun-if-env-changed=PROFILE");
    let (profile, profile_args) = if std::env::var("PROFILE") == Ok("release".to_string()) {
        ("release", &["--release"][..])
    } else {
        ("debug", &[][..])
    };

    Command::new(env!("CARGO"))
        .current_dir(impl_crate_dir.clone())
        .arg("build")
        .args(profile_args)
        .status()
        .expect("could not build mjrt/src");

    let static_lib_path = impl_crate_dir
        .join("target")
        .join(profile)
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
