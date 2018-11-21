pub use libfirm_rs_bindings as bindings;
#[macro_use]
extern crate derive_more;

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

    pub fn start_block(&self) -> Block {
        Block(unsafe { get_irg_start_block(self.irg) })
    }

    pub fn set_value<V: ValueNode>(&self, slot_idx: usize, vn: V) {
        unsafe { set_r_value(self.irg, slot_idx as i32, vn.as_value_node()) }
    }

    /// TODO: have `get_value_$mode::??` for each `mode::??`
    pub fn get_value(&self, slot_idx: usize, mode: mode::Type) -> *mut ir_node {
        unsafe { get_r_value(self.irg, slot_idx as i32, mode) }
    }

    pub fn args_node(&self) -> GraphArgs {
        unsafe { get_irg_args(self.irg) }.into()
    }

    pub fn new_imm_block<P: AsPred>(&self, pred: P) -> Block {
        let block = Block(unsafe { new_r_immBlock(self.irg) });
        block.add_pred(pred);
        block
    }

    pub fn new_const(&self, tarval: *mut ir_tarval) -> Const {
        unsafe { new_r_Const(self.irg, tarval) }.into()
    }
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

#[derive(Clone, Copy)]
pub struct Block(*mut ir_node);

impl From<*mut ir_node> for Block {
    fn from(n: *mut ir_node) -> Block {
        Block(n)
    }
}

impl Into<*mut ir_node> for Block {
    fn into(self) -> *mut ir_node {
        self.0
    }
}

pub trait AsPred {
    fn as_pred(&self) -> Pred;
}

pub trait AsSelector {
    fn as_selector(&self) -> Selector;
}

pub trait CmpOperand {
    fn as_cmp_operand(&self) -> *mut ir_node;
}

pub trait ValueNode {
    fn as_value_node(&self) -> *mut ir_node;
}

/// FIXME: remove this blanket impl because it allows invalid node types as
/// CmpOperand
impl<N> CmpOperand for N
where
    N: Into<*mut ir_node> + Copy,
{
    fn as_cmp_operand(&self) -> *mut ir_node {
        (*self).into()
    }
}

/// FIXME: remove this quasi-blanket impl because it allows invalid nodes as
/// Predi
impl AsPred for *mut ir_node {
    fn as_pred(&self) -> Pred {
        Pred(*self)
    }
}

/// FIXME: remove this quasi-blanket impl because it allows invalid nodes as
/// ValueNode
impl ValueNode for *mut ir_node {
    fn as_value_node(&self) -> *mut ir_node {
        *self
    }
}

impl Block {
    pub fn new_jmp(&self) -> Jmp {
        unsafe { new_r_Jmp(self.0) }.into()
    }
    pub fn add_pred<P: AsPred>(&self, pred: P) {
        unsafe { add_immBlock_pred(self.0, pred.as_pred().0) }
    }
    pub fn new_cond<S: AsSelector>(&self, selector: S) -> Cond {
        unsafe { new_r_Cond(self.0, selector.as_selector().0) }.into()
    }
    pub fn new_cmp<C: CmpOperand, D: CmpOperand>(
        &self,
        left: C,
        right: D,
        relation: libfirm_rs_bindings::ir_relation::Type,
    ) -> Cmp {
        unsafe {
            new_r_Cmp(
                self.0,
                left.as_cmp_operand(),
                right.as_cmp_operand(),
                relation,
            )
        }
        .into()
    }
    pub fn new_sel<P: AsPointer, I: AsIndex>(&self, p: P, i: I, array_type: Ty) -> Sel {
        unsafe { new_r_Sel(self.0, p.as_pointer(), i.as_index(), array_type.into()) }.into()
    }

    /// FIXME: either generate methods for all `ir_op` or use `ir_op` as
    /// parameter.
    pub fn new_add<A: ALUOperand, B: ALUOperand>(&self, left: A, right: B) -> ALUOpNode {
        unsafe { new_r_Add(self.0, left.as_alu_operand(), right.as_alu_operand()) }.into()
    }
}

#[derive(Clone, Copy, Into, From)]
pub struct Jmp(*mut ir_node);

#[derive(Clone, Copy, Into)]
pub struct Pred(*mut ir_node);

impl AsPred for Jmp {
    fn as_pred(&self) -> Pred {
        Pred(self.0)
    }
}

#[derive(Clone, Copy, Into, From)]
pub struct Cond(*mut ir_node);

impl Cond {
    pub fn project_true(&self) -> Jmp {
        unsafe { new_r_Proj(self.0, mode::X, pn_Cond::True) }.into()
    }
    pub fn project_false(&self) -> Jmp {
        unsafe { new_r_Proj(self.0, mode::X, pn_Cond::False) }.into()
    }
}

#[derive(Clone, Copy, Into)]
pub struct Selector(*mut ir_node);

#[derive(Clone, Copy, Into, From)]
pub struct Cmp(*mut ir_node);

impl AsSelector for Cmp {
    fn as_selector(&self) -> Selector {
        Selector(self.0)
    }
}

#[derive(Clone, Copy, Into, From)]
pub struct GraphArgs(*mut ir_node);

pub trait Projectable {
    fn ir_node(&self) -> *mut ir_node;
    fn project(
        &self,
        mode: libfirm_rs_bindings::mode::Type,
        component_number: usize,
    ) -> Projection {
        unsafe { new_r_Proj(self.ir_node(), mode, component_number as u32) }.into()
    }
}

impl Projectable for GraphArgs {
    fn ir_node(&self) -> *mut ir_node {
        self.0
    }
}

#[derive(Clone, Copy, Into, From)]
pub struct Projection(*mut ir_node);

impl ValueNode for Projection {
    fn as_value_node(&self) -> *mut ir_node {
        self.0
    }
}

pub trait AsPointer {
    fn as_pointer(&self) -> *mut ir_node;
}

/// FIXME: remove this quasi-blanket impl because it allows invalid nodes as
/// AsPointer
impl AsPointer for *mut ir_node {
    fn as_pointer(&self) -> *mut ir_node {
        *self
    }
}

pub trait AsIndex {
    fn as_index(&self) -> *mut ir_node;
}

/// FIXME: remove this quasi-blanket impl because it allows invalid nodes as
/// AsIndex
impl AsIndex for *mut ir_node {
    fn as_index(&self) -> *mut ir_node {
        *self
    }
}

/// Sel is an `ir_node` representing the result of a by-index selection.
#[derive(Clone, Copy, Into, From)]
pub struct Sel(*mut ir_node);

/// Const is an `ir_node` resulting from a new_const operation.
#[derive(Clone, Copy, Into, From)]
pub struct Const(*mut ir_node);

impl ValueNode for Const {
    fn as_value_node(&self) -> *mut ir_node {
        self.0
    }
}

pub trait ALUOperand {
    fn as_alu_operand(&self) -> *mut ir_node;
}

impl<T> ALUOperand for T
where
    T: ValueNode,
{
    fn as_alu_operand(&self) -> *mut ir_node {
        self.as_value_node()
    }
}

#[derive(Clone, Copy, Into, From)]
pub struct ALUOpNode(*mut ir_node);

impl ValueNode for ALUOpNode {
    fn as_value_node(&self) -> *mut ir_node {
        self.0
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
