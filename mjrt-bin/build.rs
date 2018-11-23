use std::path::PathBuf;

fn main() {
    // FIXME:
    // We derive the path to the libmjrt_src-*.a file from our OUT_DIR using a glob.
    // We cannot use $WORKSPACE/target/$profile/libmjrt_src.a because it is not
    // created unless a user runs `cargo build` in the workspace / mjrt-src
    // directory.
    //
    // Since we don't know which of the libmjrt_src-*.a files is the most recent, we
    // let the glob fail if there is not exactly one.
    // This is suboptimal, and may require `cargo clean` / manual removal of the
    // `libmjrt_src-*.a` files.
    //
    // What we effectively wanto to do is use a build artifact of another crate in
    // the same workspace.

    let outdir = PathBuf::from(std::env::var("OUT_DIR").expect("expecting Cargo to set OUT_DIR"));
    let deps_dir = outdir
        .ancestors()
        .nth(3)
        .expect(&format!("OUT_DIR has unexpected structure: {:?}", outdir))
        .join("deps");
    let libmjrt_src_glob = format!("{}/libmjrt_src-*.a", deps_dir.display());

    use glob::glob;
    let matches: Vec<_> = glob(&libmjrt_src_glob)
        .expect(&format!("glob({:?}) failed", libmjrt_src_glob))
        .collect();

    if matches.len() != 1 {
        panic!("libmjrt_src-*.a not exactly one match: {:?}", matches);
    }
    // safe because we checked matches len
    let static_lib_path = matches.into_iter().next().unwrap();
    println!(
        "cargo:rustc-env=MJRT_STATIC_LIB_PATH={}",
        static_lib_path.expect("glob result error").display()
    )
}
