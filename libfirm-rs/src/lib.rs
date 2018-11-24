#![allow(clippy::not_unsafe_ptr_arg_deref)]
pub use libfirm_rs_bindings as bindings;
#[macro_use]
extern crate derive_more;

use libfirm_rs_bindings::*;
use std::ffi::{CStr, CString};

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
    pub fn from_mode(mode: *mut ir_mode) -> Ty {
        unsafe { new_type_primitive(mode) }.into()
    }
    pub fn i32() -> Ty {
        unsafe { new_type_primitive(mode::Is) }.into()
    }
    pub fn bool() -> Ty {
        unsafe { new_type_primitive(mode::Bu) }.into()
    }
}

pub struct ArrayType;

impl ArrayType {
    pub fn variable_length(element_type: Ty) -> Ty {
        unsafe { new_type_array(element_type.into(), 0) }.into()
    }
    pub fn fixed_length(element_type: Ty, length: usize) -> Ty {
        unsafe { new_type_array(element_type.into(), length as u32) }.into()
    }
}

pub struct ClassType;

impl ClassType {
    pub fn new_class_type(name: &str) -> Ty {
        unsafe {
            new_type_class(CString::new(name).expect("CString::new failed").as_ptr() as *mut _)
        }
        .into()
    }
}

/// Builder for new_type_method
#[derive(Default)]
pub struct FunctionType {
    params: Vec<Ty>,
    result: Option<Ty>,
}

impl FunctionType {
    pub fn new() -> Self {
        FunctionType {
            params: Vec::new(),
            result: None,
        }
    }
    /// If the function isn't the main function don't forget to add a this
    /// param as the first param.
    pub fn add_param(&mut self, ty: Ty) {
        self.params.push(ty);
    }
    pub fn set_res(&mut self, res: Ty) {
        self.result = Some(res);
    }
    pub fn build(self, is_this_call: bool) -> Ty {
        let ft = unsafe {
            new_type_method(
                self.params.len(),
                if self.result.is_some() { 1 } else { 0 },
                false.into(), // variadic
                if is_this_call {
                    calling_convention::ThisCall
                } else {
                    cc_cdecl_set
                },
                mtp_additional_properties::NoProperty,
            )
        };
        for (i, param) in self.params.into_iter().enumerate() {
            unsafe { set_method_param_type(ft, i, param.into()) };
        }
        if let Some(res) = self.result {
            unsafe { set_method_res_type(ft, 0, res.into()) };
        }
        Ty(ft)
    }
}

#[derive(Clone,Copy,From,Into)]
pub struct Entity(*mut ir_entity);

impl Entity {
    pub fn new_global(id: &CStr, ty: Ty) -> Entity {
        unsafe {
            let global_type: *mut ir_type = get_glob_type();
            let name: *mut ident = new_id_from_str(id.as_ptr());
            new_entity(global_type, name, ty.into())
        }.into()
    }
    pub fn ty(self) -> Ty {
        unsafe { get_entity_type(self.0) }.into()
    }
    pub fn ident(&self) -> Ident {
        unsafe { get_entity_ident(self.0) }.into()
    }
    pub fn name(&self) -> &CStr {
        unsafe { CStr::from_ptr( get_entity_name(self.0)) }
    }
    pub fn ld_name(&self) -> &CStr {
        unsafe { CStr::from_ptr( get_entity_ld_name(self.0)) }
    }
}

#[derive(Clone,Copy,From,Into)]
pub struct Ident(*mut ident);

#[derive(Clone, Copy)]
pub struct Graph {
    irg: *mut ir_graph,
}

impl Graph {
    /// Create a new function entity and initialize an ir_graph for it.
    /// The entity is registered with the `get_glob_type()`, hence the name
    /// must be mangled to avoid collisions with other classes' functions.
    pub fn function(mangled_name: &str, function_type: Ty, num_slots: usize) -> Graph {
        let mangled_name = CString::new(mangled_name).expect("CString::new failed");
        unsafe {
            let entity = Entity::new_global(&mangled_name, function_type);
            let irg = new_ir_graph(entity.into(), num_slots as i32);
            Graph { irg }
        }
    }

    pub fn entity(self) -> *mut ir_entity {
        unsafe { get_irg_entity(self.irg) }
    }

    pub fn start_block(self) -> Block {
        Block(unsafe { get_irg_start_block(self.irg) })
    }

    pub fn end_block(self) -> Block {
        Block(unsafe { get_irg_end_block(self.irg) })
    }

    pub fn set_value<V: ValueNode>(self, slot_idx: usize, vn: &V) {
        unsafe { set_r_value(self.irg, slot_idx as i32, vn.as_value_node()) }
    }

    /// TODO: have `get_value_$mode::??` for each `mode::??`
    pub fn value(self, slot_idx: usize, mode: mode::Type) -> LocalVar {
        unsafe { get_r_value(self.irg, slot_idx as i32, mode) }.into()
    }

    pub fn args_node(self) -> GraphArgs {
        unsafe { get_irg_args(self.irg) }.into()
    }

    pub fn new_imm_block<P: AsPred>(self, pred: &P) -> Block {
        let block = Block(unsafe { new_r_immBlock(self.irg) });
        block.add_pred(pred);
        block
    }

    pub fn new_const(self, tarval: *mut ir_tarval) -> Const {
        unsafe { new_r_Const(self.irg, tarval) }.into()
    }

    pub fn cur_store(self) -> MemoryState {
        unsafe { get_r_store(self.irg) }.into()
    }

    pub fn cur_block(self) -> Block {
        unsafe { get_r_cur_block(self.irg) }.into()
    }

    // TODO
    pub unsafe fn set_cur_block(self, blk: Block) {
        set_r_cur_block(self.irg, blk.into())
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

impl Block {
    pub fn mature(self) {
        unsafe { mature_immBlock(self.0) }
    }
}

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
    pub fn new_jmp(self) -> Jmp {
        unsafe { new_r_Jmp(self.0) }.into()
    }
    pub fn add_pred<P: AsPred>(self, pred: &P) {
        unsafe { add_immBlock_pred(self.0, pred.as_pred().0) }
    }
    pub fn new_cond<S: AsSelector>(self, selector: &S) -> Cond {
        unsafe { new_r_Cond(self.0, selector.as_selector().0) }.into()
    }
    pub fn new_cmp<C: CmpOperand, D: CmpOperand>(
        self,
        left: &C,
        right: &D,
        relation: ir_relation::Type,
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
    pub fn new_sel<P: AsPointer, I: AsIndex>(self, p: &P, i: &I, array_type: Ty) -> Sel {
        unsafe { new_r_Sel(self.0, p.as_pointer(), i.as_index(), array_type.into()) }.into()
    }

    /// FIXME: either generate methods for all `ir_op` or use `ir_op` as
    /// parameter.
    pub fn new_add<A: ALUOperand, B: ALUOperand>(self, left: &A, right: &B) -> ALUOpNode {
        unsafe { new_r_Add(self.0, left.as_alu_operand(), right.as_alu_operand()) }.into()
    }

    /// `flags` specifies alignment, volatility and pin state. See libfirm docs.
    pub fn new_load<P: AsPointer>(
        self,
        mem: MemoryState,
        pointer: &P,
        mode: mode::Type,
        ty: Ty,
        flags: ir_cons_flags::Type,
    ) -> Load {
        unsafe {
            new_r_Load(
                self.0,
                mem.into(),
                pointer.as_pointer(),
                mode,
                ty.into(),
                flags,
            )
        }
        .into()
    }

    /// `flags` specifies alignment, volatility and pin state. See libfirm docs.
    pub fn new_store<V: ValueNode, P: AsPointer>(
        self,
        mem: MemoryState,
        pointer: &P,
        value: &V,
        ty: Ty,
        flags: ir_cons_flags::Type,
    ) -> Store {
        unsafe {
            new_r_Store(
                self.0,
                mem.into(),
                pointer.as_pointer(),
                value.as_value_node(),
                ty.into(),
                flags,
            )
        }
        .into()
    }

    pub fn new_return(self, mem: MemoryState, ret: Option<*mut ir_node>) -> Return {
        let (arity, inputs) = match ret {
            Some(node) => (1, vec![node]),
            None => (0, vec![]),
        };
        unsafe { new_r_Return(self.0, mem.into(), arity, inputs.as_ptr()) }.into()
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
    pub fn project_true(self) -> Jmp {
        unsafe { new_r_Proj(self.0, mode::X, pn_Cond::True) }.into()
    }
    pub fn project_false(self) -> Jmp {
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

impl AsSelector for Selector {
    fn as_selector(&self) -> Selector {
        *self
    }
}

#[derive(Clone, Copy, Into, From)]
pub struct GraphArgs(*mut ir_node);

pub trait Projectable {
    fn ir_node(&self) -> *mut ir_node;
    fn project(&self, mode: mode::Type, component_number: usize) -> Projection {
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

impl AsPointer for Sel {
    fn as_pointer(&self) -> *mut ir_node {
        self.0
    }
}

/// Const is an `ir_node` resulting from a new_const operation.
#[derive(Clone, Copy, Into, From)]
pub struct Const(*mut ir_node);

#[derive(Clone, Copy, Into, From)]
pub struct Return(*mut ir_node);

#[derive(Clone, Copy, Into, From)]
pub struct LocalVar(*mut ir_node);

impl AsPred for Return {
    fn as_pred(&self) -> Pred {
        Pred(self.0)
    }
}

impl ValueNode for Const {
    fn as_value_node(&self) -> *mut ir_node {
        self.0
    }
}

impl ValueNode for LocalVar {
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

#[derive(Clone, Copy, Into, From)]
pub struct MemoryState(*mut ir_node);

/// Node representing a load operation.
#[derive(Clone, Copy, Into, From)]
pub struct Load(*mut ir_node);

/// The value of a projection of `Load`.
#[derive(Clone, Copy, Into, From)]
pub struct LoadValue(*mut ir_node);

impl ValueNode for LoadValue {
    fn as_value_node(&self) -> *mut ir_node {
        self.0
    }
}

impl Load {
    /// TODO can ret type be `MemoryState` ?
    pub fn project_mem(self) -> *mut ir_node {
        unsafe { new_r_Proj(self.0, mode::M, pn_Load::M) }
    }
    pub fn project_res(self, mode: mode::Type) -> LoadValue {
        unsafe { new_r_Proj(self.0, mode, pn_Load::Res) }.into()
    }
}

#[derive(Clone, Copy, Into, From)]
pub struct Store(*mut ir_node);

#[derive(Clone, Copy, Into, From)]
pub struct StoreValue(*mut ir_node);

impl Store {
    /// TODO can ret type be `MemoryState` ?
    pub fn project_mem(self) -> *mut ir_node {
        unsafe { new_r_Proj(self.0, mode::M, pn_Store::M) }
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
