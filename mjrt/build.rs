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

    let features: Vec<_> = std::env::vars()
        .filter_map(|(var, _)| {
            const CARGO_FEATURE: &'static str = "CARGO_FEATURE_";
            if var.starts_with(CARGO_FEATURE) {
                let feature = var.trim_start_matches(CARGO_FEATURE);
                Some((var.to_owned(), feature.to_owned()))
            } else {
                None
            }
        })
        .collect();

    for (var, _) in &features {
        println!("cargo:rerun-if-env-changed={}", var);
    }

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

    let feature_args = features
        .iter()
        .fold(String::new(), |s, (_, f)| s + &f.to_lowercase() + " ");

    println!(
        "invoking cargo build with features {:?} in mjrt-impl crate at {:?}",
        feature_args, impl_crate_dir
    );

    println!("cargo:rerun-if-env-changed=PROFILE");
    let (profile, profile_args) = if std::env::var("PROFILE") == Ok("release".to_string()) {
        ("release", &["--release"][..])
    } else {
        ("debug", &[][..])
    };

    let mut c = Command::new(env!("CARGO"));
    c.current_dir(impl_crate_dir.clone())
        .arg("build")
        .args(profile_args)
        .arg("--features")
        .arg(feature_args);
    println!(
        "invoking cargo build\n\tin mjrt-impl crate dir {:?}\n\tcommand: {:?}",
        impl_crate_dir, c
    );
    c.status().expect("could not build mjrt-impl");

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
