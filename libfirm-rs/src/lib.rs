pub use libfirm_rs_bindings as bindings;

use libfirm_rs_bindings::ir_type;

#[derive(Clone, Copy)]
pub struct Ty(*mut ir_type);

impl Into<*mut ir_type> for Ty {
    fn into(self) -> *mut ir_type {
        self.0
    }
}

impl From<*mut ir_type> for Ty {
    fn from(primitive: *mut ir_type) -> Ty {
        Ty(primitive)
    }
}

/// Builder for new_type_method
pub struct FunctionType {
    params: Vec<Ty>,
    results: Vec<Ty>,
}

impl FunctionType {
    pub fn new() -> Self {
        FunctionType {
            params: Vec::new(),
            results: Vec::new(),
        }
    }
    pub fn param(mut self, ty: Ty) -> FunctionType {
        self.params.push(ty);
        self
    }
    pub fn res(mut self, res: Ty) -> FunctionType {
        self.results.push(res);
        self
    }
    pub fn build(self) -> Ty {
        use libfirm_rs_bindings::*;
        let ft = unsafe {
            new_type_method(
                self.params.len(),
                self.results.len(),
                false.into(), // variadic
                cc_cdecl_set,
                mtp_additional_properties::NoProperty,
            )
        };
        for (i, param) in self.params.into_iter().enumerate() {
            unsafe { set_method_param_type(ft, i, param.into()) };
        }
        for (i, res) in self.results.into_iter().enumerate() {
            unsafe { set_method_res_type(ft, i, res.into()) };
        }
        Ty(ft)
    }
}

#[cfg(test)]
mod tests {

    use libfirm_rs_bindings;

    #[test]
    fn init() {
        unsafe { libfirm_rs_bindings::ir_init() };
    }
}
