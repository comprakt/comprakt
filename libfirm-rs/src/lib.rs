pub use libfirm_rs_bindings as bindings;

use libfirm_rs_bindings::*;
use std::ffi::CString;

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

impl Ty {
    pub fn pointer(self) -> Ty {
        unsafe { new_type_pointer(self.into()) }.into()
    }
}

pub struct PrimitiveType;
impl PrimitiveType {
    pub fn from_mode(mode: *mut libfirm_rs_bindings::ir_mode) -> Ty {
        unsafe { new_type_primitive(mode) }.into()
    }
    pub fn isize() -> Ty {
        unsafe { new_type_primitive(mode::Is) }.into()
    }
    pub fn usize() -> Ty {
        unsafe { new_type_primitive(mode::Iu) }.into()
    }
}

pub struct ArrayType;

impl ArrayType {
    pub fn varlength(element_type: Ty) -> Ty {
        unsafe { new_type_array(element_type.into(), 0) }.into()
    }
    pub fn fixedlength(element_type: Ty, length: usize) -> Ty {
        unsafe { new_type_array(element_type.into(), length as u32) }.into()
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

#[derive(Clone, Copy)]
pub struct Graph {
    entity: *mut ir_entity,
    irg: *mut ir_graph,
}

impl Graph {
    /// Create a new function entity and initialize an ir_graph for it.
    /// The entity is registered with the `get_glob_type()`, hence the name
    /// must be mangled to avoid collisions with other classes' functions.
    pub fn function(mangled_name: String, function_type: Ty, num_slots: usize) -> Graph {
        let mangled_name =
            CString::new(mangled_name).expect("mangled_name expected to be valid CString");
        unsafe {
            let global_type: *mut ir_type = get_glob_type();
            let name: *mut ident = new_id_from_str(mangled_name.as_ptr());
            let entity = new_entity(global_type, name, function_type.into());
            let irg = new_ir_graph(entity, num_slots as i32);
            Graph { entity, irg }
        }
    }

    pub fn start_block(&self) -> Node { unsafe{ get_irg_start_block(self.irg) }.into() }

}

impl Into<*mut ir_graph> for Graph {
    fn into(self) -> *mut ir_graph {
        self.irg
    }
}

impl Into<*const ir_graph> for Graph {
    fn into(self) -> *const ir_graph {
        self.irg as *const _
    }
}

#[derive(Clone,Copy)]
pub struct Node(*mut ir_node);

impl From<*mut ir_node> for Node {
    fn from(n: *mut ir_node) -> Node { Node(n) }
}

impl Into<*mut ir_node> for Node {
    fn into(self) -> *mut ir_node { self.0 }
}

#[cfg(test)]
mod tests {

    use libfirm_rs_bindings;

    #[test]
    fn init() {
        unsafe { libfirm_rs_bindings::ir_init() };
    }
}
