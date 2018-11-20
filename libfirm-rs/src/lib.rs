pub use libfirm_rs_bindings as bindings;

#[cfg(test)]
mod tests {

    use libfirm_rs_bindings;

    #[test]
    fn init() {
        unsafe { libfirm_rs_bindings::ir_init() };
    }
}
