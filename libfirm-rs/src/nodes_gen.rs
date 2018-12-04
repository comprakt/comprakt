use libfirm_rs_bindings as bindings;
use std::collections::HashMap;
use super::nodes::NodeTrait;

#[derive(Debug)]
pub enum Node {
    Add(Add),
    Address(Address),
    Align(Align),
    Alloc(Alloc),
    Anchor(Anchor),
    And(And),
    Bad(Bad),
    Bitcast(Bitcast),
    Block(Block),
    Builtin(Builtin),
    Call(Call),
    Cmp(Cmp),
    Cond(Cond),
    Confirm(Confirm),
    Const(Const),
    Conv(Conv),
    CopyB(CopyB),
    Deleted(Deleted),
    Div(Div),
    Dummy(Dummy),
    End(End),
    Eor(Eor),
    Free(Free),
    IJmp(IJmp),
    Id(Id),
    Jmp(Jmp),
    Load(Load),
    Member(Member),
    Minus(Minus),
    Mod(Mod),
    Mul(Mul),
    Mulh(Mulh),
    Mux(Mux),
    NoMem(NoMem),
    Not(Not),
    Offset(Offset),
    Or(Or),
    Phi(Phi),
    Pin(Pin),
    Proj(Proj),
    Raise(Raise),
    Return(Return),
    Sel(Sel),
    Shl(Shl),
    Shr(Shr),
    Shrs(Shrs),
    Size(Size),
    Start(Start),
    Store(Store),
    Sub(Sub),
    Switch(Switch),
    Sync(Sync),
    Tuple(Tuple),
    Unknown(Unknown),
}

impl NodeTrait for Node {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        match self {
            Node::Add(node) => node.internal_ir_node(),
            Node::Address(node) => node.internal_ir_node(),
            Node::Align(node) => node.internal_ir_node(),
            Node::Alloc(node) => node.internal_ir_node(),
            Node::Anchor(node) => node.internal_ir_node(),
            Node::And(node) => node.internal_ir_node(),
            Node::Bad(node) => node.internal_ir_node(),
            Node::Bitcast(node) => node.internal_ir_node(),
            Node::Block(node) => node.internal_ir_node(),
            Node::Builtin(node) => node.internal_ir_node(),
            Node::Call(node) => node.internal_ir_node(),
            Node::Cmp(node) => node.internal_ir_node(),
            Node::Cond(node) => node.internal_ir_node(),
            Node::Confirm(node) => node.internal_ir_node(),
            Node::Const(node) => node.internal_ir_node(),
            Node::Conv(node) => node.internal_ir_node(),
            Node::CopyB(node) => node.internal_ir_node(),
            Node::Deleted(node) => node.internal_ir_node(),
            Node::Div(node) => node.internal_ir_node(),
            Node::Dummy(node) => node.internal_ir_node(),
            Node::End(node) => node.internal_ir_node(),
            Node::Eor(node) => node.internal_ir_node(),
            Node::Free(node) => node.internal_ir_node(),
            Node::IJmp(node) => node.internal_ir_node(),
            Node::Id(node) => node.internal_ir_node(),
            Node::Jmp(node) => node.internal_ir_node(),
            Node::Load(node) => node.internal_ir_node(),
            Node::Member(node) => node.internal_ir_node(),
            Node::Minus(node) => node.internal_ir_node(),
            Node::Mod(node) => node.internal_ir_node(),
            Node::Mul(node) => node.internal_ir_node(),
            Node::Mulh(node) => node.internal_ir_node(),
            Node::Mux(node) => node.internal_ir_node(),
            Node::NoMem(node) => node.internal_ir_node(),
            Node::Not(node) => node.internal_ir_node(),
            Node::Offset(node) => node.internal_ir_node(),
            Node::Or(node) => node.internal_ir_node(),
            Node::Phi(node) => node.internal_ir_node(),
            Node::Pin(node) => node.internal_ir_node(),
            Node::Proj(node) => node.internal_ir_node(),
            Node::Raise(node) => node.internal_ir_node(),
            Node::Return(node) => node.internal_ir_node(),
            Node::Sel(node) => node.internal_ir_node(),
            Node::Shl(node) => node.internal_ir_node(),
            Node::Shr(node) => node.internal_ir_node(),
            Node::Shrs(node) => node.internal_ir_node(),
            Node::Size(node) => node.internal_ir_node(),
            Node::Start(node) => node.internal_ir_node(),
            Node::Store(node) => node.internal_ir_node(),
            Node::Sub(node) => node.internal_ir_node(),
            Node::Switch(node) => node.internal_ir_node(),
            Node::Sync(node) => node.internal_ir_node(),
            Node::Tuple(node) => node.internal_ir_node(),
            Node::Unknown(node) => node.internal_ir_node(),
        }
    }
}

type NodeFactoryFn = fn(*mut bindings::ir_node) -> Node;
pub struct NodeFactory(HashMap<u32, NodeFactoryFn>);
impl NodeFactory {
    pub fn new() -> Self {
        let mut map = HashMap::<u32, NodeFactoryFn>::new();
        unsafe {
            let op = bindings::get_op_Add();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_add);
            let op = bindings::get_op_Address();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_address);
            let op = bindings::get_op_Align();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_align);
            let op = bindings::get_op_Alloc();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_alloc);
            let op = bindings::get_op_Anchor();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_anchor);
            let op = bindings::get_op_And();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_and);
            let op = bindings::get_op_Bad();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_bad);
            let op = bindings::get_op_Bitcast();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_bitcast);
            let op = bindings::get_op_Block();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_block);
            let op = bindings::get_op_Builtin();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_builtin);
            let op = bindings::get_op_Call();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_call);
            let op = bindings::get_op_Cmp();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_cmp);
            let op = bindings::get_op_Cond();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_cond);
            let op = bindings::get_op_Confirm();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_confirm);
            let op = bindings::get_op_Const();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_const);
            let op = bindings::get_op_Conv();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_conv);
            let op = bindings::get_op_CopyB();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_copyb);
            let op = bindings::get_op_Deleted();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_deleted);
            let op = bindings::get_op_Div();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_div);
            let op = bindings::get_op_Dummy();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_dummy);
            let op = bindings::get_op_End();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_end);
            let op = bindings::get_op_Eor();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_eor);
            let op = bindings::get_op_Free();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_free);
            let op = bindings::get_op_IJmp();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_ijmp);
            let op = bindings::get_op_Id();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_id);
            let op = bindings::get_op_Jmp();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_jmp);
            let op = bindings::get_op_Load();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_load);
            let op = bindings::get_op_Member();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_member);
            let op = bindings::get_op_Minus();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_minus);
            let op = bindings::get_op_Mod();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_mod);
            let op = bindings::get_op_Mul();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_mul);
            let op = bindings::get_op_Mulh();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_mulh);
            let op = bindings::get_op_Mux();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_mux);
            let op = bindings::get_op_NoMem();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_nomem);
            let op = bindings::get_op_Not();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_not);
            let op = bindings::get_op_Offset();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_offset);
            let op = bindings::get_op_Or();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_or);
            let op = bindings::get_op_Phi();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_phi);
            let op = bindings::get_op_Pin();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_pin);
            let op = bindings::get_op_Proj();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_proj);
            let op = bindings::get_op_Raise();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_raise);
            let op = bindings::get_op_Return();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_return);
            let op = bindings::get_op_Sel();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_sel);
            let op = bindings::get_op_Shl();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_shl);
            let op = bindings::get_op_Shr();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_shr);
            let op = bindings::get_op_Shrs();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_shrs);
            let op = bindings::get_op_Size();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_size);
            let op = bindings::get_op_Start();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_start);
            let op = bindings::get_op_Store();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_store);
            let op = bindings::get_op_Sub();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_sub);
            let op = bindings::get_op_Switch();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_switch);
            let op = bindings::get_op_Sync();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_sync);
            let op = bindings::get_op_Tuple();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_tuple);
            let op = bindings::get_op_Unknown();
            let op_code = bindings::get_op_code(op);
            map.insert(op_code, Self::create_unknown);
        }
        NodeFactory(map)
    }

    pub fn node(ir_node: *mut bindings::ir_node) -> Node {
        Self::new().create(ir_node)
    }

    pub fn create(&self, ir_node: *mut bindings::ir_node) -> Node {
        let op_code = unsafe { bindings::get_irn_opcode(ir_node) };
        let f = self.0.get(&op_code).unwrap();
        f(ir_node)
    }

    fn create_add(ir_node: *mut bindings::ir_node) -> Node {
        Node::Add(Add(ir_node))
    }
    fn create_address(ir_node: *mut bindings::ir_node) -> Node {
        Node::Address(Address(ir_node))
    }
    fn create_align(ir_node: *mut bindings::ir_node) -> Node {
        Node::Align(Align(ir_node))
    }
    fn create_alloc(ir_node: *mut bindings::ir_node) -> Node {
        Node::Alloc(Alloc(ir_node))
    }
    fn create_anchor(ir_node: *mut bindings::ir_node) -> Node {
        Node::Anchor(Anchor(ir_node))
    }
    fn create_and(ir_node: *mut bindings::ir_node) -> Node {
        Node::And(And(ir_node))
    }
    fn create_bad(ir_node: *mut bindings::ir_node) -> Node {
        Node::Bad(Bad(ir_node))
    }
    fn create_bitcast(ir_node: *mut bindings::ir_node) -> Node {
        Node::Bitcast(Bitcast(ir_node))
    }
    fn create_block(ir_node: *mut bindings::ir_node) -> Node {
        Node::Block(Block(ir_node))
    }
    fn create_builtin(ir_node: *mut bindings::ir_node) -> Node {
        Node::Builtin(Builtin(ir_node))
    }
    fn create_call(ir_node: *mut bindings::ir_node) -> Node {
        Node::Call(Call(ir_node))
    }
    fn create_cmp(ir_node: *mut bindings::ir_node) -> Node {
        Node::Cmp(Cmp(ir_node))
    }
    fn create_cond(ir_node: *mut bindings::ir_node) -> Node {
        Node::Cond(Cond(ir_node))
    }
    fn create_confirm(ir_node: *mut bindings::ir_node) -> Node {
        Node::Confirm(Confirm(ir_node))
    }
    fn create_const(ir_node: *mut bindings::ir_node) -> Node {
        Node::Const(Const(ir_node))
    }
    fn create_conv(ir_node: *mut bindings::ir_node) -> Node {
        Node::Conv(Conv(ir_node))
    }
    fn create_copyb(ir_node: *mut bindings::ir_node) -> Node {
        Node::CopyB(CopyB(ir_node))
    }
    fn create_deleted(ir_node: *mut bindings::ir_node) -> Node {
        Node::Deleted(Deleted(ir_node))
    }
    fn create_div(ir_node: *mut bindings::ir_node) -> Node {
        Node::Div(Div(ir_node))
    }
    fn create_dummy(ir_node: *mut bindings::ir_node) -> Node {
        Node::Dummy(Dummy(ir_node))
    }
    fn create_end(ir_node: *mut bindings::ir_node) -> Node {
        Node::End(End(ir_node))
    }
    fn create_eor(ir_node: *mut bindings::ir_node) -> Node {
        Node::Eor(Eor(ir_node))
    }
    fn create_free(ir_node: *mut bindings::ir_node) -> Node {
        Node::Free(Free(ir_node))
    }
    fn create_ijmp(ir_node: *mut bindings::ir_node) -> Node {
        Node::IJmp(IJmp(ir_node))
    }
    fn create_id(ir_node: *mut bindings::ir_node) -> Node {
        Node::Id(Id(ir_node))
    }
    fn create_jmp(ir_node: *mut bindings::ir_node) -> Node {
        Node::Jmp(Jmp(ir_node))
    }
    fn create_load(ir_node: *mut bindings::ir_node) -> Node {
        Node::Load(Load(ir_node))
    }
    fn create_member(ir_node: *mut bindings::ir_node) -> Node {
        Node::Member(Member(ir_node))
    }
    fn create_minus(ir_node: *mut bindings::ir_node) -> Node {
        Node::Minus(Minus(ir_node))
    }
    fn create_mod(ir_node: *mut bindings::ir_node) -> Node {
        Node::Mod(Mod(ir_node))
    }
    fn create_mul(ir_node: *mut bindings::ir_node) -> Node {
        Node::Mul(Mul(ir_node))
    }
    fn create_mulh(ir_node: *mut bindings::ir_node) -> Node {
        Node::Mulh(Mulh(ir_node))
    }
    fn create_mux(ir_node: *mut bindings::ir_node) -> Node {
        Node::Mux(Mux(ir_node))
    }
    fn create_nomem(ir_node: *mut bindings::ir_node) -> Node {
        Node::NoMem(NoMem(ir_node))
    }
    fn create_not(ir_node: *mut bindings::ir_node) -> Node {
        Node::Not(Not(ir_node))
    }
    fn create_offset(ir_node: *mut bindings::ir_node) -> Node {
        Node::Offset(Offset(ir_node))
    }
    fn create_or(ir_node: *mut bindings::ir_node) -> Node {
        Node::Or(Or(ir_node))
    }
    fn create_phi(ir_node: *mut bindings::ir_node) -> Node {
        Node::Phi(Phi(ir_node))
    }
    fn create_pin(ir_node: *mut bindings::ir_node) -> Node {
        Node::Pin(Pin(ir_node))
    }
    fn create_proj(ir_node: *mut bindings::ir_node) -> Node {
        Node::Proj(Proj(ir_node))
    }
    fn create_raise(ir_node: *mut bindings::ir_node) -> Node {
        Node::Raise(Raise(ir_node))
    }
    fn create_return(ir_node: *mut bindings::ir_node) -> Node {
        Node::Return(Return(ir_node))
    }
    fn create_sel(ir_node: *mut bindings::ir_node) -> Node {
        Node::Sel(Sel(ir_node))
    }
    fn create_shl(ir_node: *mut bindings::ir_node) -> Node {
        Node::Shl(Shl(ir_node))
    }
    fn create_shr(ir_node: *mut bindings::ir_node) -> Node {
        Node::Shr(Shr(ir_node))
    }
    fn create_shrs(ir_node: *mut bindings::ir_node) -> Node {
        Node::Shrs(Shrs(ir_node))
    }
    fn create_size(ir_node: *mut bindings::ir_node) -> Node {
        Node::Size(Size(ir_node))
    }
    fn create_start(ir_node: *mut bindings::ir_node) -> Node {
        Node::Start(Start(ir_node))
    }
    fn create_store(ir_node: *mut bindings::ir_node) -> Node {
        Node::Store(Store(ir_node))
    }
    fn create_sub(ir_node: *mut bindings::ir_node) -> Node {
        Node::Sub(Sub(ir_node))
    }
    fn create_switch(ir_node: *mut bindings::ir_node) -> Node {
        Node::Switch(Switch(ir_node))
    }
    fn create_sync(ir_node: *mut bindings::ir_node) -> Node {
        Node::Sync(Sync(ir_node))
    }
    fn create_tuple(ir_node: *mut bindings::ir_node) -> Node {
        Node::Tuple(Tuple(ir_node))
    }
    fn create_unknown(ir_node: *mut bindings::ir_node) -> Node {
        Node::Unknown(Unknown(ir_node))
    }
}

/// returns the sum of its operands
#[derive(Debug)]
pub struct Add(*mut bindings::ir_node);

impl Add {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Add_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Add_left(self.0, unwrapped); }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Add_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Add_right(self.0, unwrapped); }
    }

}

impl Into<Node> for Add {
    fn into(self) -> Node {
        Node::Add(self)
    }
}

impl NodeTrait for Add {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Symbolic constant that represents the address of an entity (variable or
/// method)
#[derive(Debug)]
pub struct Address(*mut bindings::ir_node);

impl Address {
}

impl Into<Node> for Address {
    fn into(self) -> Node {
        Node::Address(self)
    }
}

impl NodeTrait for Address {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// A symbolic constant that represents the alignment of a type
#[derive(Debug)]
pub struct Align(*mut bindings::ir_node);

impl Align {
}

impl Into<Node> for Align {
    fn into(self) -> Node {
        Node::Align(self)
    }
}

impl NodeTrait for Align {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Allocates a block of memory on the stack.
#[derive(Debug)]
pub struct Alloc(*mut bindings::ir_node);

impl Alloc {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Alloc_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Alloc_mem(self.0, unwrapped); }
    }

    /// Gets size of the block in bytes.
    pub fn size(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Alloc_size(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets size of the block in bytes.
    pub fn set_size(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Alloc_size(self.0, unwrapped); }
    }

    /// Gets alignment of the memory block (must be a power of 2).
    pub fn alignment(&self) -> ::std::os::raw::c_uint {
        let unwrapped = unsafe { bindings::get_Alloc_alignment(self.0) };
        unwrapped
    }

    /// Sets alignment of the memory block (must be a power of 2).
    pub fn set_alignment(&self, val: ::std::os::raw::c_uint) {
        let unwrapped = val;
        unsafe { bindings::set_Alloc_alignment(self.0, unwrapped); }
    }

}

impl Into<Node> for Alloc {
    fn into(self) -> Node {
        Node::Alloc(self)
    }
}

impl NodeTrait for Alloc {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Utility node used to "hold" nodes in a graph that might possibly not be
/// reachable by other means or which should be reachable immediately without
/// searching through the graph.
/// Each firm-graph contains exactly one anchor node whose address is always
/// known. All other well-known graph-nodes like Start, End, NoMem, ...
/// are found by looking at the respective Anchor operand.
#[derive(Debug)]
pub struct Anchor(*mut bindings::ir_node);

impl Anchor {
    /// Gets block the end node belongs to.
    pub fn end_block(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_end_block(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets block the end node belongs to.
    pub fn set_end_block(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Anchor_end_block(self.0, unwrapped); }
    }

    /// Gets block the start node belongs to.
    pub fn start_block(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_start_block(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets block the start node belongs to.
    pub fn set_start_block(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Anchor_start_block(self.0, unwrapped); }
    }

    /// Gets end node of this ir_graph.
    pub fn end(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_end(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets end node of this ir_graph.
    pub fn set_end(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Anchor_end(self.0, unwrapped); }
    }

    /// Gets start node of this ir_graph.
    pub fn start(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_start(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets start node of this ir_graph.
    pub fn set_start(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Anchor_start(self.0, unwrapped); }
    }

    /// Gets frame of this ir_graph.
    pub fn frame(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_frame(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets frame of this ir_graph.
    pub fn set_frame(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Anchor_frame(self.0, unwrapped); }
    }

    /// Gets initial memory of this ir_graph.
    pub fn initial_mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_initial_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets initial memory of this ir_graph.
    pub fn set_initial_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Anchor_initial_mem(self.0, unwrapped); }
    }

    /// Gets argument proj of the start node.
    pub fn args(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_args(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets argument proj of the start node.
    pub fn set_args(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Anchor_args(self.0, unwrapped); }
    }

    /// Gets the only NoMem node of this ir_graph.
    pub fn no_mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_no_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets the only NoMem node of this ir_graph.
    pub fn set_no_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Anchor_no_mem(self.0, unwrapped); }
    }

}

impl Into<Node> for Anchor {
    fn into(self) -> Node {
        Node::Anchor(self)
    }
}

impl NodeTrait for Anchor {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// returns the result of a bitwise and operation of its operands
#[derive(Debug)]
pub struct And(*mut bindings::ir_node);

impl And {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_And_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_And_left(self.0, unwrapped); }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_And_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_And_right(self.0, unwrapped); }
    }

}

impl Into<Node> for And {
    fn into(self) -> Node {
        Node::And(self)
    }
}

impl NodeTrait for And {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Bad nodes indicate invalid input, which is values which should never be
/// computed.
/// 
/// The typical use case for the Bad node is removing unreachable code.
/// Frontends should set the current_block to Bad when it is clear that
/// following code must be unreachable (i.e. after a goto or return statement).
/// Optimizations also set block predecessors to Bad when it becomes clear,
/// that a control flow edge can never be executed.
/// 
/// The gigo optimizations ensures that nodes with Bad as their block, get
/// replaced by Bad themselves. Nodes with at least 1 Bad input get exchanged
/// with Bad too. Exception to this rule are Block, Phi, Tuple and End node;
/// This is because removing inputs from a Block is hairy operation (requiring,
/// Phis to be shortened too for example). So instead of removing block inputs
/// they are set to Bad, and the actual removal is left to the control flow
/// optimization phase. Block, Phi, Tuple with only Bad inputs however are
/// replaced by Bad right away.
/// 
/// In the future we may use the Bad node to model poison values that arise
/// from undefined behaviour like reading uninitialized local variables in C.
#[derive(Debug)]
pub struct Bad(*mut bindings::ir_node);

impl Bad {
}

impl Into<Node> for Bad {
    fn into(self) -> Node {
        Node::Bad(self)
    }
}

impl NodeTrait for Bad {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Converts a value between modes with different arithmetics but same
/// number of bits by reinterpreting the bits in the new mode
#[derive(Debug)]
pub struct Bitcast(*mut bindings::ir_node);

impl Bitcast {
    /// Gets operand.
    pub fn op(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Bitcast_op(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets operand.
    pub fn set_op(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Bitcast_op(self.0, unwrapped); }
    }

}

impl Into<Node> for Bitcast {
    fn into(self) -> Node {
        Node::Bitcast(self)
    }
}

impl NodeTrait for Bitcast {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// A basic block
#[derive(Debug)]
pub struct Block(*mut bindings::ir_node);

impl Block {
    /// Gets entity representing this block.
    pub fn entity(&self) -> *mut bindings::ir_entity {
        let unwrapped = unsafe { bindings::get_Block_entity(self.0) };
        unwrapped
    }

    /// Sets entity representing this block.
    pub fn set_entity(&self, val: *mut bindings::ir_entity) {
        let unwrapped = val;
        unsafe { bindings::set_Block_entity(self.0, unwrapped); }
    }

}

impl Into<Node> for Block {
    fn into(self) -> Node {
        Node::Block(self)
    }
}

impl NodeTrait for Block {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// performs a backend-specific builtin.
#[derive(Debug)]
pub struct Builtin(*mut bindings::ir_node);

impl Builtin {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Builtin_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Builtin_mem(self.0, unwrapped); }
    }

    /// Gets kind of builtin.
    pub fn kind(&self) -> bindings::ir_builtin_kind::Type {
        let unwrapped = unsafe { bindings::get_Builtin_kind(self.0) };
        unwrapped
    }

    /// Sets kind of builtin.
    pub fn set_kind(&self, val: bindings::ir_builtin_kind::Type) {
        let unwrapped = val;
        unsafe { bindings::set_Builtin_kind(self.0, unwrapped); }
    }

    /// Gets method type for the builtin call.
    pub fn ty(&self) -> *mut bindings::ir_type {
        let unwrapped = unsafe { bindings::get_Builtin_type(self.0) };
        unwrapped
    }

    /// Sets method type for the builtin call.
    pub fn set_ty(&self, val: *mut bindings::ir_type) {
        let unwrapped = val;
        unsafe { bindings::set_Builtin_type(self.0, unwrapped); }
    }

}

impl Into<Node> for Builtin {
    fn into(self) -> Node {
        Node::Builtin(self)
    }
}

impl NodeTrait for Builtin {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Calls other code. Control flow is transferred to ptr, additional
/// operands are passed to the called code. Called code usually performs a
/// return operation. The operands of this return operation are the result
/// of the Call node.
#[derive(Debug)]
pub struct Call(*mut bindings::ir_node);

impl Call {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Call_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Call_mem(self.0, unwrapped); }
    }

    /// Gets pointer to called code.
    pub fn ptr(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Call_ptr(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets pointer to called code.
    pub fn set_ptr(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Call_ptr(self.0, unwrapped); }
    }

    /// Gets type of the call (usually type of the called procedure).
    pub fn ty(&self) -> *mut bindings::ir_type {
        let unwrapped = unsafe { bindings::get_Call_type(self.0) };
        unwrapped
    }

    /// Sets type of the call (usually type of the called procedure).
    pub fn set_ty(&self, val: *mut bindings::ir_type) {
        let unwrapped = val;
        unsafe { bindings::set_Call_type(self.0, unwrapped); }
    }

}

impl Into<Node> for Call {
    fn into(self) -> Node {
        Node::Call(self)
    }
}

impl NodeTrait for Call {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Compares its two operands and checks whether a specified
/// relation (like less or equal) is fulfilled.
#[derive(Debug)]
pub struct Cmp(*mut bindings::ir_node);

impl Cmp {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Cmp_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Cmp_left(self.0, unwrapped); }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Cmp_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Cmp_right(self.0, unwrapped); }
    }

    /// Gets Comparison relation.
    pub fn relation(&self) -> bindings::ir_relation::Type {
        let unwrapped = unsafe { bindings::get_Cmp_relation(self.0) };
        unwrapped
    }

    /// Sets Comparison relation.
    pub fn set_relation(&self, val: bindings::ir_relation::Type) {
        let unwrapped = val;
        unsafe { bindings::set_Cmp_relation(self.0, unwrapped); }
    }

}

impl Into<Node> for Cmp {
    fn into(self) -> Node {
        Node::Cmp(self)
    }
}

impl NodeTrait for Cmp {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Conditionally change control flow.
#[derive(Debug)]
pub struct Cond(*mut bindings::ir_node);

impl Cond {
    /// Gets condition parameter.
    pub fn selector(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Cond_selector(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets condition parameter.
    pub fn set_selector(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Cond_selector(self.0, unwrapped); }
    }

    /// Gets can indicate the most likely jump.
    pub fn jmp_pred(&self) -> bindings::cond_jmp_predicate::Type {
        let unwrapped = unsafe { bindings::get_Cond_jmp_pred(self.0) };
        unwrapped
    }

    /// Sets can indicate the most likely jump.
    pub fn set_jmp_pred(&self, val: bindings::cond_jmp_predicate::Type) {
        let unwrapped = val;
        unsafe { bindings::set_Cond_jmp_pred(self.0, unwrapped); }
    }

}

impl Into<Node> for Cond {
    fn into(self) -> Node {
        Node::Cond(self)
    }
}

impl NodeTrait for Cond {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Specifies constraints for a value. This allows explicit representation
/// of path-sensitive properties. (Example: This value is always >= 0 on 1
/// if-branch then all users within that branch are rerouted to a confirm-node
/// specifying this property).
/// 
/// A constraint is specified for the relation between value and bound.
/// value is always returned.
/// Note that this node does NOT check or assert the constraint, it merely
/// specifies it.
#[derive(Debug)]
pub struct Confirm(*mut bindings::ir_node);

impl Confirm {
    /// Gets value to express a constraint for.
    pub fn value(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Confirm_value(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets value to express a constraint for.
    pub fn set_value(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Confirm_value(self.0, unwrapped); }
    }

    /// Gets value to compare against.
    pub fn bound(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Confirm_bound(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets value to compare against.
    pub fn set_bound(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Confirm_bound(self.0, unwrapped); }
    }

    /// Gets relation of value to bound.
    pub fn relation(&self) -> bindings::ir_relation::Type {
        let unwrapped = unsafe { bindings::get_Confirm_relation(self.0) };
        unwrapped
    }

    /// Sets relation of value to bound.
    pub fn set_relation(&self, val: bindings::ir_relation::Type) {
        let unwrapped = val;
        unsafe { bindings::set_Confirm_relation(self.0, unwrapped); }
    }

}

impl Into<Node> for Confirm {
    fn into(self) -> Node {
        Node::Confirm(self)
    }
}

impl NodeTrait for Confirm {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Returns a constant value.
#[derive(Debug)]
pub struct Const(*mut bindings::ir_node);

impl Const {
    /// Gets constant value (a tarval object).
    pub fn tarval(&self) -> *mut bindings::ir_tarval {
        let unwrapped = unsafe { bindings::get_Const_tarval(self.0) };
        unwrapped
    }

    /// Sets constant value (a tarval object).
    pub fn set_tarval(&self, val: *mut bindings::ir_tarval) {
        let unwrapped = val;
        unsafe { bindings::set_Const_tarval(self.0, unwrapped); }
    }

}

impl Into<Node> for Const {
    fn into(self) -> Node {
        Node::Const(self)
    }
}

impl NodeTrait for Const {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Converts values between modes
#[derive(Debug)]
pub struct Conv(*mut bindings::ir_node);

impl Conv {
    /// Gets operand.
    pub fn op(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Conv_op(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets operand.
    pub fn set_op(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Conv_op(self.0, unwrapped); }
    }

}

impl Into<Node> for Conv {
    fn into(self) -> Node {
        Node::Conv(self)
    }
}

impl NodeTrait for Conv {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Copies a block of memory with statically known size/type.
#[derive(Debug)]
pub struct CopyB(*mut bindings::ir_node);

impl CopyB {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_CopyB_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_CopyB_mem(self.0, unwrapped); }
    }

    /// Gets destination address.
    pub fn dst(&self) -> Node {
        let unwrapped = unsafe { bindings::get_CopyB_dst(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets destination address.
    pub fn set_dst(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_CopyB_dst(self.0, unwrapped); }
    }

    /// Gets source address.
    pub fn src(&self) -> Node {
        let unwrapped = unsafe { bindings::get_CopyB_src(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets source address.
    pub fn set_src(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_CopyB_src(self.0, unwrapped); }
    }

    /// Gets type of copied data.
    pub fn ty(&self) -> *mut bindings::ir_type {
        let unwrapped = unsafe { bindings::get_CopyB_type(self.0) };
        unwrapped
    }

    /// Sets type of copied data.
    pub fn set_ty(&self, val: *mut bindings::ir_type) {
        let unwrapped = val;
        unsafe { bindings::set_CopyB_type(self.0, unwrapped); }
    }

    /// Gets volatile CopyB nodes have a visible side-effect and may not be optimized.
    pub fn volatility(&self) -> bindings::ir_volatility::Type {
        let unwrapped = unsafe { bindings::get_CopyB_volatility(self.0) };
        unwrapped
    }

    /// Sets volatile CopyB nodes have a visible side-effect and may not be optimized.
    pub fn set_volatility(&self, val: bindings::ir_volatility::Type) {
        let unwrapped = val;
        unsafe { bindings::set_CopyB_volatility(self.0, unwrapped); }
    }

}

impl Into<Node> for CopyB {
    fn into(self) -> Node {
        Node::CopyB(self)
    }
}

impl NodeTrait for CopyB {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Internal node which is temporary set to nodes which are already removed
/// from the graph.
#[derive(Debug)]
pub struct Deleted(*mut bindings::ir_node);

impl Deleted {
}

impl Into<Node> for Deleted {
    fn into(self) -> Node {
        Node::Deleted(self)
    }
}

impl NodeTrait for Deleted {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// returns the quotient of its 2 operands
#[derive(Debug)]
pub struct Div(*mut bindings::ir_node);

impl Div {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Div_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Div_mem(self.0, unwrapped); }
    }

    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Div_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Div_left(self.0, unwrapped); }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Div_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Div_right(self.0, unwrapped); }
    }

    /// Gets mode of the result value.
    pub fn resmode(&self) -> *mut bindings::ir_mode {
        let unwrapped = unsafe { bindings::get_Div_resmode(self.0) };
        unwrapped
    }

    /// Sets mode of the result value.
    pub fn set_resmode(&self, val: *mut bindings::ir_mode) {
        let unwrapped = val;
        unsafe { bindings::set_Div_resmode(self.0, unwrapped); }
    }

    /// Gets Set when division remainder is known to be zero.
    pub fn no_remainder(&self) -> i32 {
        let unwrapped = unsafe { bindings::get_Div_no_remainder(self.0) };
        unwrapped
    }

    /// Sets Set when division remainder is known to be zero.
    pub fn set_no_remainder(&self, val: i32) {
        let unwrapped = val;
        unsafe { bindings::set_Div_no_remainder(self.0, unwrapped); }
    }

}

impl Into<Node> for Div {
    fn into(self) -> Node {
        Node::Div(self)
    }
}

impl NodeTrait for Div {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// A placeholder value. This is used when constructing cyclic graphs where
/// you have cases where not all predecessors of a phi-node are known. Dummy
/// nodes are used for the unknown predecessors and replaced later.
#[derive(Debug)]
pub struct Dummy(*mut bindings::ir_node);

impl Dummy {
}

impl Into<Node> for Dummy {
    fn into(self) -> Node {
        Node::Dummy(self)
    }
}

impl NodeTrait for Dummy {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Last node of a graph. It references nodes in endless loops (so called
/// keepalive edges)
#[derive(Debug)]
pub struct End(*mut bindings::ir_node);

impl End {
}

impl Into<Node> for End {
    fn into(self) -> Node {
        Node::End(self)
    }
}

impl NodeTrait for End {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// returns the result of a bitwise exclusive or operation of its operands.
/// 
/// This is also known as the Xor operation.
#[derive(Debug)]
pub struct Eor(*mut bindings::ir_node);

impl Eor {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Eor_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Eor_left(self.0, unwrapped); }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Eor_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Eor_right(self.0, unwrapped); }
    }

}

impl Into<Node> for Eor {
    fn into(self) -> Node {
        Node::Eor(self)
    }
}

impl NodeTrait for Eor {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Frees a block of memory previously allocated by an Alloc node
#[derive(Debug)]
pub struct Free(*mut bindings::ir_node);

impl Free {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Free_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Free_mem(self.0, unwrapped); }
    }

    /// Gets pointer to the object to free.
    pub fn ptr(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Free_ptr(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets pointer to the object to free.
    pub fn set_ptr(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Free_ptr(self.0, unwrapped); }
    }

}

impl Into<Node> for Free {
    fn into(self) -> Node {
        Node::Free(self)
    }
}

impl NodeTrait for Free {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Jumps to the code in its argument. The code has to be in the same
/// function and the destination must be one of the blocks reachable
/// by the tuple results
#[derive(Debug)]
pub struct IJmp(*mut bindings::ir_node);

impl IJmp {
    /// Gets target address of the jump.
    pub fn target(&self) -> Node {
        let unwrapped = unsafe { bindings::get_IJmp_target(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets target address of the jump.
    pub fn set_target(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_IJmp_target(self.0, unwrapped); }
    }

}

impl Into<Node> for IJmp {
    fn into(self) -> Node {
        Node::IJmp(self)
    }
}

impl NodeTrait for IJmp {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Returns its operand unchanged.
/// 
/// This is mainly used when exchanging nodes. Usually you shouldn't see Id
/// nodes since the getters/setters for node inputs skip them automatically.
#[derive(Debug)]
pub struct Id(*mut bindings::ir_node);

impl Id {
    /// Gets the value which is returned unchanged.
    pub fn pred(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Id_pred(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets the value which is returned unchanged.
    pub fn set_pred(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Id_pred(self.0, unwrapped); }
    }

}

impl Into<Node> for Id {
    fn into(self) -> Node {
        Node::Id(self)
    }
}

impl NodeTrait for Id {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Jumps to the block connected through the out-value
#[derive(Debug)]
pub struct Jmp(*mut bindings::ir_node);

impl Jmp {
}

impl Into<Node> for Jmp {
    fn into(self) -> Node {
        Node::Jmp(self)
    }
}

impl NodeTrait for Jmp {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Loads a value from memory (heap or stack).
#[derive(Debug)]
pub struct Load(*mut bindings::ir_node);

impl Load {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Load_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Load_mem(self.0, unwrapped); }
    }

    /// Gets address to load from.
    pub fn ptr(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Load_ptr(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets address to load from.
    pub fn set_ptr(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Load_ptr(self.0, unwrapped); }
    }

    /// Gets mode of the value to be loaded.
    pub fn mode(&self) -> *mut bindings::ir_mode {
        let unwrapped = unsafe { bindings::get_Load_mode(self.0) };
        unwrapped
    }

    /// Sets mode of the value to be loaded.
    pub fn set_mode(&self, val: *mut bindings::ir_mode) {
        let unwrapped = val;
        unsafe { bindings::set_Load_mode(self.0, unwrapped); }
    }

    /// Gets The type of the object which is stored at ptr (need not match with mode).
    pub fn ty(&self) -> *mut bindings::ir_type {
        let unwrapped = unsafe { bindings::get_Load_type(self.0) };
        unwrapped
    }

    /// Sets The type of the object which is stored at ptr (need not match with mode).
    pub fn set_ty(&self, val: *mut bindings::ir_type) {
        let unwrapped = val;
        unsafe { bindings::set_Load_type(self.0, unwrapped); }
    }

    /// Gets volatile loads are a visible side-effect and may not be optimized.
    pub fn volatility(&self) -> bindings::ir_volatility::Type {
        let unwrapped = unsafe { bindings::get_Load_volatility(self.0) };
        unwrapped
    }

    /// Sets volatile loads are a visible side-effect and may not be optimized.
    pub fn set_volatility(&self, val: bindings::ir_volatility::Type) {
        let unwrapped = val;
        unsafe { bindings::set_Load_volatility(self.0, unwrapped); }
    }

    /// Gets pointers to unaligned loads don't need to respect the load-mode/type alignments.
    pub fn unaligned(&self) -> bindings::ir_align::Type {
        let unwrapped = unsafe { bindings::get_Load_unaligned(self.0) };
        unwrapped
    }

    /// Sets pointers to unaligned loads don't need to respect the load-mode/type alignments.
    pub fn set_unaligned(&self, val: bindings::ir_align::Type) {
        let unwrapped = val;
        unsafe { bindings::set_Load_unaligned(self.0, unwrapped); }
    }

}

impl Into<Node> for Load {
    fn into(self) -> Node {
        Node::Load(self)
    }
}

impl NodeTrait for Load {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Computes the address of a compound type member given the base address
/// of an instance of the compound type.
/// 
/// A Member node must only produce a NULL pointer if the ptr input is NULL.
#[derive(Debug)]
pub struct Member(*mut bindings::ir_node);

impl Member {
    /// Gets pointer to object to select from.
    pub fn ptr(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Member_ptr(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets pointer to object to select from.
    pub fn set_ptr(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Member_ptr(self.0, unwrapped); }
    }

    /// Gets entity which is selected.
    pub fn entity(&self) -> *mut bindings::ir_entity {
        let unwrapped = unsafe { bindings::get_Member_entity(self.0) };
        unwrapped
    }

    /// Sets entity which is selected.
    pub fn set_entity(&self, val: *mut bindings::ir_entity) {
        let unwrapped = val;
        unsafe { bindings::set_Member_entity(self.0, unwrapped); }
    }

}

impl Into<Node> for Member {
    fn into(self) -> Node {
        Node::Member(self)
    }
}

impl NodeTrait for Member {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// returns the additive inverse of its operand
#[derive(Debug)]
pub struct Minus(*mut bindings::ir_node);

impl Minus {
    /// Gets operand.
    pub fn op(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Minus_op(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets operand.
    pub fn set_op(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Minus_op(self.0, unwrapped); }
    }

}

impl Into<Node> for Minus {
    fn into(self) -> Node {
        Node::Minus(self)
    }
}

impl NodeTrait for Minus {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// returns the remainder of its operands from an implied division.
/// 
/// Examples:
/// 
/// * mod(5,3)   produces 2
/// * mod(5,-3)  produces 2
/// * mod(-5,3)  produces -2
/// * mod(-5,-3) produces -2
#[derive(Debug)]
pub struct Mod(*mut bindings::ir_node);

impl Mod {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Mod_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Mod_mem(self.0, unwrapped); }
    }

    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Mod_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Mod_left(self.0, unwrapped); }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Mod_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Mod_right(self.0, unwrapped); }
    }

    /// Gets mode of the result.
    pub fn resmode(&self) -> *mut bindings::ir_mode {
        let unwrapped = unsafe { bindings::get_Mod_resmode(self.0) };
        unwrapped
    }

    /// Sets mode of the result.
    pub fn set_resmode(&self, val: *mut bindings::ir_mode) {
        let unwrapped = val;
        unsafe { bindings::set_Mod_resmode(self.0, unwrapped); }
    }

}

impl Into<Node> for Mod {
    fn into(self) -> Node {
        Node::Mod(self)
    }
}

impl NodeTrait for Mod {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// returns the product of its operands
#[derive(Debug)]
pub struct Mul(*mut bindings::ir_node);

impl Mul {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Mul_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Mul_left(self.0, unwrapped); }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Mul_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Mul_right(self.0, unwrapped); }
    }

}

impl Into<Node> for Mul {
    fn into(self) -> Node {
        Node::Mul(self)
    }
}

impl NodeTrait for Mul {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// returns the upper word of the product of its operands (the part which
/// would not fit into the result mode of a normal Mul anymore)
#[derive(Debug)]
pub struct Mulh(*mut bindings::ir_node);

impl Mulh {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Mulh_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Mulh_left(self.0, unwrapped); }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Mulh_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Mulh_right(self.0, unwrapped); }
    }

}

impl Into<Node> for Mulh {
    fn into(self) -> Node {
        Node::Mulh(self)
    }
}

impl NodeTrait for Mulh {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// returns the false or true operand depending on the value of the sel
/// operand
#[derive(Debug)]
pub struct Mux(*mut bindings::ir_node);

impl Mux {
    /// Gets value making the output selection.
    pub fn sel(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Mux_sel(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets value making the output selection.
    pub fn set_sel(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Mux_sel(self.0, unwrapped); }
    }

    /// Gets selected if sel input is false.
    pub fn false_(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Mux_false(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets selected if sel input is false.
    pub fn set_false_(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Mux_false(self.0, unwrapped); }
    }

    /// Gets selected if sel input is true.
    pub fn true_(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Mux_true(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets selected if sel input is true.
    pub fn set_true_(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Mux_true(self.0, unwrapped); }
    }

}

impl Into<Node> for Mux {
    fn into(self) -> Node {
        Node::Mux(self)
    }
}

impl NodeTrait for Mux {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Placeholder node for cases where you don't need any memory input
#[derive(Debug)]
pub struct NoMem(*mut bindings::ir_node);

impl NoMem {
}

impl Into<Node> for NoMem {
    fn into(self) -> Node {
        Node::NoMem(self)
    }
}

impl NodeTrait for NoMem {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// returns the bitwise complement of a value. Works for boolean values, too.
#[derive(Debug)]
pub struct Not(*mut bindings::ir_node);

impl Not {
    /// Gets operand.
    pub fn op(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Not_op(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets operand.
    pub fn set_op(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Not_op(self.0, unwrapped); }
    }

}

impl Into<Node> for Not {
    fn into(self) -> Node {
        Node::Not(self)
    }
}

impl NodeTrait for Not {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Symbolic constant that represents the offset of an entity in its owner type.
#[derive(Debug)]
pub struct Offset(*mut bindings::ir_node);

impl Offset {
}

impl Into<Node> for Offset {
    fn into(self) -> Node {
        Node::Offset(self)
    }
}

impl NodeTrait for Offset {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// returns the result of a bitwise or operation of its operands
#[derive(Debug)]
pub struct Or(*mut bindings::ir_node);

impl Or {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Or_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Or_left(self.0, unwrapped); }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Or_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Or_right(self.0, unwrapped); }
    }

}

impl Into<Node> for Or {
    fn into(self) -> Node {
        Node::Or(self)
    }
}

impl NodeTrait for Or {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Choose a value based on control flow. A phi node has 1 input for each
/// predecessor of its block. If a block is entered from its nth predecessor
/// all phi nodes produce their nth input as result.
#[derive(Debug)]
pub struct Phi(*mut bindings::ir_node);

impl Phi {
    /// Gets whether Phi represents the observable effect of a (possibly) nonterminating loop.
    pub fn loop_(&self) -> i32 {
        let unwrapped = unsafe { bindings::get_Phi_loop(self.0) };
        unwrapped
    }

    /// Sets whether Phi represents the observable effect of a (possibly) nonterminating loop.
    pub fn set_loop_(&self, val: i32) {
        let unwrapped = val;
        unsafe { bindings::set_Phi_loop(self.0, unwrapped); }
    }

}

impl Into<Node> for Phi {
    fn into(self) -> Node {
        Node::Phi(self)
    }
}

impl NodeTrait for Phi {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Pin the value of the node node in the current block. No users of the Pin
/// node can float above the Block of the Pin. The node cannot float behind
/// this block. Often used to Pin the NoMem node.
#[derive(Debug)]
pub struct Pin(*mut bindings::ir_node);

impl Pin {
    /// Gets value which is pinned.
    pub fn op(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Pin_op(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets value which is pinned.
    pub fn set_op(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Pin_op(self.0, unwrapped); }
    }

}

impl Into<Node> for Pin {
    fn into(self) -> Node {
        Node::Pin(self)
    }
}

impl NodeTrait for Pin {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// returns an entry of a tuple value
#[derive(Debug)]
pub struct Proj(*mut bindings::ir_node);

impl Proj {
    /// Gets the tuple value from which a part is extracted.
    pub fn pred(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Proj_pred(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets the tuple value from which a part is extracted.
    pub fn set_pred(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Proj_pred(self.0, unwrapped); }
    }

    /// Gets number of tuple component to be extracted.
    pub fn num(&self) -> ::std::os::raw::c_uint {
        let unwrapped = unsafe { bindings::get_Proj_num(self.0) };
        unwrapped
    }

    /// Sets number of tuple component to be extracted.
    pub fn set_num(&self, val: ::std::os::raw::c_uint) {
        let unwrapped = val;
        unsafe { bindings::set_Proj_num(self.0, unwrapped); }
    }

}

impl Into<Node> for Proj {
    fn into(self) -> Node {
        Node::Proj(self)
    }
}

impl NodeTrait for Proj {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Raises an exception. Unconditional change of control flow. Writes an
/// explicit Except variable to memory to pass it to the exception handler.
/// Must be lowered to a Call to a runtime check function.
#[derive(Debug)]
pub struct Raise(*mut bindings::ir_node);

impl Raise {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Raise_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Raise_mem(self.0, unwrapped); }
    }

    /// Gets pointer to exception object to be thrown.
    pub fn exo_ptr(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Raise_exo_ptr(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets pointer to exception object to be thrown.
    pub fn set_exo_ptr(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Raise_exo_ptr(self.0, unwrapped); }
    }

}

impl Into<Node> for Raise {
    fn into(self) -> Node {
        Node::Raise(self)
    }
}

impl NodeTrait for Raise {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Returns from the current function. Takes memory and return values as
/// operands.
#[derive(Debug)]
pub struct Return(*mut bindings::ir_node);

impl Return {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Return_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Return_mem(self.0, unwrapped); }
    }

}

impl Into<Node> for Return {
    fn into(self) -> Node {
        Node::Return(self)
    }
}

impl NodeTrait for Return {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Computes the address of an array element from the array base pointer and
/// an index.
/// 
/// A Sel node must only produce a NULL pointer if the ptr input is NULL.
#[derive(Debug)]
pub struct Sel(*mut bindings::ir_node);

impl Sel {
    /// Gets pointer to array to select from.
    pub fn ptr(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Sel_ptr(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets pointer to array to select from.
    pub fn set_ptr(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Sel_ptr(self.0, unwrapped); }
    }

    /// Gets index to select.
    pub fn index(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Sel_index(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets index to select.
    pub fn set_index(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Sel_index(self.0, unwrapped); }
    }

    /// Gets array type.
    pub fn ty(&self) -> *mut bindings::ir_type {
        let unwrapped = unsafe { bindings::get_Sel_type(self.0) };
        unwrapped
    }

    /// Sets array type.
    pub fn set_ty(&self, val: *mut bindings::ir_type) {
        let unwrapped = val;
        unsafe { bindings::set_Sel_type(self.0, unwrapped); }
    }

}

impl Into<Node> for Sel {
    fn into(self) -> Node {
        Node::Sel(self)
    }
}

impl NodeTrait for Sel {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Returns its first operands bits shifted left by the amount of the 2nd
/// operand.
/// The right input (shift amount) must be an unsigned integer value.
/// If the result mode has modulo_shift!=0, then the effective shift amount is
/// the right input modulo this modulo_shift amount.
#[derive(Debug)]
pub struct Shl(*mut bindings::ir_node);

impl Shl {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Shl_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Shl_left(self.0, unwrapped); }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Shl_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Shl_right(self.0, unwrapped); }
    }

}

impl Into<Node> for Shl {
    fn into(self) -> Node {
        Node::Shl(self)
    }
}

impl NodeTrait for Shl {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Returns its first operands bits shifted right by the amount of the 2nd
/// operand. No special handling for the sign bit is performed (zero extension).
/// The right input (shift amount) must be an unsigned integer value.
/// If the result mode has modulo_shift!=0, then the effective shift amount is
/// the right input modulo this modulo_shift amount.
#[derive(Debug)]
pub struct Shr(*mut bindings::ir_node);

impl Shr {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Shr_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Shr_left(self.0, unwrapped); }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Shr_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Shr_right(self.0, unwrapped); }
    }

}

impl Into<Node> for Shr {
    fn into(self) -> Node {
        Node::Shr(self)
    }
}

impl NodeTrait for Shr {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Returns its first operands bits shifted right by the amount of the 2nd
/// operand. The leftmost bit (usually the sign bit) stays the same
/// (sign extension).
/// The right input (shift amount) must be an unsigned integer value.
/// If the result mode has modulo_shift!=0, then the effective shift amount is
/// the right input modulo this modulo_shift amount.
#[derive(Debug)]
pub struct Shrs(*mut bindings::ir_node);

impl Shrs {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Shrs_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Shrs_left(self.0, unwrapped); }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Shrs_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Shrs_right(self.0, unwrapped); }
    }

}

impl Into<Node> for Shrs {
    fn into(self) -> Node {
        Node::Shrs(self)
    }
}

impl NodeTrait for Shrs {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// A symbolic constant that represents the size of a type
#[derive(Debug)]
pub struct Size(*mut bindings::ir_node);

impl Size {
}

impl Into<Node> for Size {
    fn into(self) -> Node {
        Node::Size(self)
    }
}

impl NodeTrait for Size {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// The first node of a graph. Execution starts with this node.
#[derive(Debug)]
pub struct Start(*mut bindings::ir_node);

impl Start {
}

impl Into<Node> for Start {
    fn into(self) -> Node {
        Node::Start(self)
    }
}

impl NodeTrait for Start {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Stores a value into memory (heap or stack).
#[derive(Debug)]
pub struct Store(*mut bindings::ir_node);

impl Store {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Store_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Store_mem(self.0, unwrapped); }
    }

    /// Gets address to store to.
    pub fn ptr(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Store_ptr(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets address to store to.
    pub fn set_ptr(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Store_ptr(self.0, unwrapped); }
    }

    /// Gets value to store.
    pub fn value(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Store_value(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets value to store.
    pub fn set_value(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Store_value(self.0, unwrapped); }
    }

    /// Gets The type of the object which is stored at ptr (need not match with value's type).
    pub fn ty(&self) -> *mut bindings::ir_type {
        let unwrapped = unsafe { bindings::get_Store_type(self.0) };
        unwrapped
    }

    /// Sets The type of the object which is stored at ptr (need not match with value's type).
    pub fn set_ty(&self, val: *mut bindings::ir_type) {
        let unwrapped = val;
        unsafe { bindings::set_Store_type(self.0, unwrapped); }
    }

    /// Gets volatile stores are a visible side-effect and may not be optimized.
    pub fn volatility(&self) -> bindings::ir_volatility::Type {
        let unwrapped = unsafe { bindings::get_Store_volatility(self.0) };
        unwrapped
    }

    /// Sets volatile stores are a visible side-effect and may not be optimized.
    pub fn set_volatility(&self, val: bindings::ir_volatility::Type) {
        let unwrapped = val;
        unsafe { bindings::set_Store_volatility(self.0, unwrapped); }
    }

    /// Gets pointers to unaligned stores don't need to respect the load-mode/type alignments.
    pub fn unaligned(&self) -> bindings::ir_align::Type {
        let unwrapped = unsafe { bindings::get_Store_unaligned(self.0) };
        unwrapped
    }

    /// Sets pointers to unaligned stores don't need to respect the load-mode/type alignments.
    pub fn set_unaligned(&self, val: bindings::ir_align::Type) {
        let unwrapped = val;
        unsafe { bindings::set_Store_unaligned(self.0, unwrapped); }
    }

}

impl Into<Node> for Store {
    fn into(self) -> Node {
        Node::Store(self)
    }
}

impl NodeTrait for Store {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// returns the difference of its operands
#[derive(Debug)]
pub struct Sub(*mut bindings::ir_node);

impl Sub {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Sub_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Sub_left(self.0, unwrapped); }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Sub_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Sub_right(self.0, unwrapped); }
    }

}

impl Into<Node> for Sub {
    fn into(self) -> Node {
        Node::Sub(self)
    }
}

impl NodeTrait for Sub {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Change control flow. The destination is chosen based on an integer
/// input value which is looked up in a table.
/// 
/// Backends can implement this efficiently using a jump table.
#[derive(Debug)]
pub struct Switch(*mut bindings::ir_node);

impl Switch {
    /// Gets input selector.
    pub fn selector(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Switch_selector(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets input selector.
    pub fn set_selector(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe { bindings::set_Switch_selector(self.0, unwrapped); }
    }

    /// Gets number of outputs (including pn_Switch_default).
    pub fn n_outs(&self) -> ::std::os::raw::c_uint {
        let unwrapped = unsafe { bindings::get_Switch_n_outs(self.0) };
        unwrapped
    }

    /// Sets number of outputs (including pn_Switch_default).
    pub fn set_n_outs(&self, val: ::std::os::raw::c_uint) {
        let unwrapped = val;
        unsafe { bindings::set_Switch_n_outs(self.0, unwrapped); }
    }

    /// Gets table describing mapping from input values to Proj numbers.
    pub fn table(&self) -> *mut bindings::ir_switch_table {
        let unwrapped = unsafe { bindings::get_Switch_table(self.0) };
        unwrapped
    }

    /// Sets table describing mapping from input values to Proj numbers.
    pub fn set_table(&self, val: *mut bindings::ir_switch_table) {
        let unwrapped = val;
        unsafe { bindings::set_Switch_table(self.0, unwrapped); }
    }

}

impl Into<Node> for Switch {
    fn into(self) -> Node {
        Node::Switch(self)
    }
}

impl NodeTrait for Switch {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// The Sync operation unifies several partial memory blocks. These blocks
/// have to be pairwise disjunct or the values in common locations have to
/// be identical.  This operation allows to specify all operations that
/// eventually need several partial memory blocks as input with a single
/// entrance by unifying the memories with a preceding Sync operation.
#[derive(Debug)]
pub struct Sync(*mut bindings::ir_node);

impl Sync {
}

impl Into<Node> for Sync {
    fn into(self) -> Node {
        Node::Sync(self)
    }
}

impl NodeTrait for Sync {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Builds a Tuple from single values.
/// 
/// This is needed to implement optimizations that remove a node that produced
/// a tuple.  The node can be replaced by the Tuple operation so that the
/// following Proj nodes have not to be changed. (They are hard to find due to
/// the implementation with pointers in only one direction.) The Tuple node is
/// smaller than any other node, so that a node can be changed into a Tuple by
/// just changing its opcode and giving it a new in array.
#[derive(Debug)]
pub struct Tuple(*mut bindings::ir_node);

impl Tuple {
}

impl Into<Node> for Tuple {
    fn into(self) -> Node {
        Node::Tuple(self)
    }
}

impl NodeTrait for Tuple {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

/// Returns an unknown (at compile- and runtime) value. It is a valid
/// optimization to replace an Unknown by any other constant value.
/// 
/// Be careful when optimising Unknown values, you cannot simply replace
/// Unknown+x or Unknown<x with a new Unknown node if there are multiple
/// users of the original unknown node!
#[derive(Debug)]
pub struct Unknown(*mut bindings::ir_node);

impl Unknown {
}

impl Into<Node> for Unknown {
    fn into(self) -> Node {
        Node::Unknown(self)
    }
}

impl NodeTrait for Unknown {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

