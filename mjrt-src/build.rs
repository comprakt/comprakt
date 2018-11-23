use std::path::PathBuf;
fn main() {
    // /abs/path/to/workspace/target/debug/build/mjrt-src-8d44f7ebd6a1c0a1/out
    let out_dir = std::env::var("OUT_DIR").expect("expecting OUT_DIR to be set by Cargo");
    let out_dir = PathBuf::from(out_dir);
    let build_profile_dir = out_dir.ancestors().nth(3).expect("unexpected format of OUT_DIR"); // see expected format in comment above
    let libpath = build_profile_dir.join("libmjrt_src.a");
    println!("cargo:STATIC={}", libpath.display());
    println!("cargo:OUTDIR={}", out_dir.display());
}
