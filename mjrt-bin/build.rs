use std::path::PathBuf;
fn main() {
    for (k,v) in std::env::vars() {
        println!("{}: {}", k, v)
    }
    let static_lib_path = std::env::var("DEP_MJRT_STATIC")
        .expect("mjrt-src crate should export DEP_MJRT_STATIC via the 'links' Cargo.toml->package key mechanism");
//    println!("cargo:rustc-env=MJRT_STATIC_LIB_PATH={}", static_lib_path)
    let outdir = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    let deps_dir = outdir.ancestors().nth(3).unwrap().join("deps");
    let libmjrt_src_glob = format!("{}/libmjrt_src-*.a", deps_dir.display());

    use glob::glob;
    let matches: Vec<_> = glob(&libmjrt_src_glob).unwrap().collect();
    if matches.len() != 1 {
        panic!("libmjrt_src-*.a not exactly one match: {:?}", matches);
    }
    let static_lib_path = matches.into_iter().next().unwrap();
    println!("cargo:rustc-env=MJRT_STATIC_LIB_PATH={}", static_lib_path.unwrap().display())

}
