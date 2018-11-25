pub const STATIC_LIB: &[u8] = include_bytes!(env!("MJRT_STATIC_LIB_PATH"));

pub const LINKER_FLAGS: &[&str] = &["-pthread", "-ldl"];
