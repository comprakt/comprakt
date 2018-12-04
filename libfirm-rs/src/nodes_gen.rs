use libfirm_rs_bindings as bindings;
use std::collections::HashMap;

#[derive(Debug)]
pub enum Node {
    ASM(ASMNode),
    Add(AddNode),
    Address(AddressNode),
    Align(AlignNode),
    Alloc(AllocNode),
    Anchor(AnchorNode),
    And(AndNode),
    Bad(BadNode),
    Bitcast(BitcastNode),
    Block(BlockNode),
    Builtin(BuiltinNode),
    Call(CallNode),
    Cmp(CmpNode),
    Cond(CondNode),
    Confirm(ConfirmNode),
    Const(ConstNode),
    Conv(ConvNode),
    CopyB(CopyBNode),
    Deleted(DeletedNode),
    Div(DivNode),
    Dummy(DummyNode),
    End(EndNode),
    Eor(EorNode),
    Free(FreeNode),
    IJmp(IJmpNode),
    Id(IdNode),
    Jmp(JmpNode),
    Load(LoadNode),
    Member(MemberNode),
    Minus(MinusNode),
    Mod(ModNode),
    Mul(MulNode),
    Mulh(MulhNode),
    Mux(MuxNode),
    NoMem(NoMemNode),
    Not(NotNode),
    Offset(OffsetNode),
    Or(OrNode),
    Phi(PhiNode),
    Pin(PinNode),
    Proj(ProjNode),
    Raise(RaiseNode),
    Return(ReturnNode),
    Sel(SelNode),
    Shl(ShlNode),
    Shr(ShrNode),
    Shrs(ShrsNode),
    Size(SizeNode),
    Start(StartNode),
    Store(StoreNode),
    Sub(SubNode),
    Switch(SwitchNode),
    Sync(SyncNode),
    Tuple(TupleNode),
    Unknown(UnknownNode),
}

impl Into<*const ir_node> for Node {
    fn into(self) -> *const ir_node {
        match self {
            ASM(node) => node.into(),
            Add(node) => node.into(),
            Address(node) => node.into(),
            Align(node) => node.into(),
            Alloc(node) => node.into(),
            Anchor(node) => node.into(),
            And(node) => node.into(),
            Bad(node) => node.into(),
            Bitcast(node) => node.into(),
            Block(node) => node.into(),
            Builtin(node) => node.into(),
            Call(node) => node.into(),
            Cmp(node) => node.into(),
            Cond(node) => node.into(),
            Confirm(node) => node.into(),
            Const(node) => node.into(),
            Conv(node) => node.into(),
            CopyB(node) => node.into(),
            Deleted(node) => node.into(),
            Div(node) => node.into(),
            Dummy(node) => node.into(),
            End(node) => node.into(),
            Eor(node) => node.into(),
            Free(node) => node.into(),
            IJmp(node) => node.into(),
            Id(node) => node.into(),
            Jmp(node) => node.into(),
            Load(node) => node.into(),
            Member(node) => node.into(),
            Minus(node) => node.into(),
            Mod(node) => node.into(),
            Mul(node) => node.into(),
            Mulh(node) => node.into(),
            Mux(node) => node.into(),
            NoMem(node) => node.into(),
            Not(node) => node.into(),
            Offset(node) => node.into(),
            Or(node) => node.into(),
            Phi(node) => node.into(),
            Pin(node) => node.into(),
            Proj(node) => node.into(),
            Raise(node) => node.into(),
            Return(node) => node.into(),
            Sel(node) => node.into(),
            Shl(node) => node.into(),
            Shr(node) => node.into(),
            Shrs(node) => node.into(),
            Size(node) => node.into(),
            Start(node) => node.into(),
            Store(node) => node.into(),
            Sub(node) => node.into(),
            Switch(node) => node.into(),
            Sync(node) => node.into(),
            Tuple(node) => node.into(),
            Unknown(node) => node.into(),
        }
    }
}
struct NodeFactory(HashMap<i32, Fn(*const ir_node) -> Node>);
impl NodeFactory {
    pub fn new() -> Self {
        let map = HashMap::new();
        let op = bindings::get_op_ASM();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_asm);
        let op = bindings::get_op_Add();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_add);
        let op = bindings::get_op_Address();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_address);
        let op = bindings::get_op_Align();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_align);
        let op = bindings::get_op_Alloc();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_alloc);
        let op = bindings::get_op_Anchor();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_anchor);
        let op = bindings::get_op_And();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_and);
        let op = bindings::get_op_Bad();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_bad);
        let op = bindings::get_op_Bitcast();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_bitcast);
        let op = bindings::get_op_Block();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_block);
        let op = bindings::get_op_Builtin();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_builtin);
        let op = bindings::get_op_Call();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_call);
        let op = bindings::get_op_Cmp();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_cmp);
        let op = bindings::get_op_Cond();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_cond);
        let op = bindings::get_op_Confirm();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_confirm);
        let op = bindings::get_op_Const();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_const);
        let op = bindings::get_op_Conv();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_conv);
        let op = bindings::get_op_CopyB();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_copyb);
        let op = bindings::get_op_Deleted();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_deleted);
        let op = bindings::get_op_Div();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_div);
        let op = bindings::get_op_Dummy();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_dummy);
        let op = bindings::get_op_End();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_end);
        let op = bindings::get_op_Eor();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_eor);
        let op = bindings::get_op_Free();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_free);
        let op = bindings::get_op_IJmp();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_ijmp);
        let op = bindings::get_op_Id();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_id);
        let op = bindings::get_op_Jmp();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_jmp);
        let op = bindings::get_op_Load();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_load);
        let op = bindings::get_op_Member();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_member);
        let op = bindings::get_op_Minus();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_minus);
        let op = bindings::get_op_Mod();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_mod);
        let op = bindings::get_op_Mul();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_mul);
        let op = bindings::get_op_Mulh();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_mulh);
        let op = bindings::get_op_Mux();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_mux);
        let op = bindings::get_op_NoMem();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_nomem);
        let op = bindings::get_op_Not();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_not);
        let op = bindings::get_op_Offset();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_offset);
        let op = bindings::get_op_Or();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_or);
        let op = bindings::get_op_Phi();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_phi);
        let op = bindings::get_op_Pin();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_pin);
        let op = bindings::get_op_Proj();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_proj);
        let op = bindings::get_op_Raise();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_raise);
        let op = bindings::get_op_Return();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_return);
        let op = bindings::get_op_Sel();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_sel);
        let op = bindings::get_op_Shl();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_shl);
        let op = bindings::get_op_Shr();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_shr);
        let op = bindings::get_op_Shrs();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_shrs);
        let op = bindings::get_op_Size();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_size);
        let op = bindings::get_op_Start();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_start);
        let op = bindings::get_op_Store();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_store);
        let op = bindings::get_op_Sub();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_sub);
        let op = bindings::get_op_Switch();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_switch);
        let op = bindings::get_op_Sync();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_sync);
        let op = bindings::get_op_Tuple();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_tuple);
        let op = bindings::get_op_Unknown();
        let op_code = bindings::get_op_code(op);
        map.insert(op_code, create_unknown);
        NodeFactory(map)
    }

    pub fn node(*const ir_node) -> Node {
        Self::new().create(ir_node)
    }

    pub fn create(&self, *const ir_node) -> Node {
        let op_code = bindings::get_irn_opcode(ir_node);
        let f = self.0.get(op_code).unwrap();
        f()
    }

    fn create_asm(ir_node: *const ir_node) -> Node {
        Node::ASM(ASMNode(ir_node))
    }
    fn create_add(ir_node: *const ir_node) -> Node {
        Node::Add(AddNode(ir_node))
    }
    fn create_address(ir_node: *const ir_node) -> Node {
        Node::Address(AddressNode(ir_node))
    }
    fn create_align(ir_node: *const ir_node) -> Node {
        Node::Align(AlignNode(ir_node))
    }
    fn create_alloc(ir_node: *const ir_node) -> Node {
        Node::Alloc(AllocNode(ir_node))
    }
    fn create_anchor(ir_node: *const ir_node) -> Node {
        Node::Anchor(AnchorNode(ir_node))
    }
    fn create_and(ir_node: *const ir_node) -> Node {
        Node::And(AndNode(ir_node))
    }
    fn create_bad(ir_node: *const ir_node) -> Node {
        Node::Bad(BadNode(ir_node))
    }
    fn create_bitcast(ir_node: *const ir_node) -> Node {
        Node::Bitcast(BitcastNode(ir_node))
    }
    fn create_block(ir_node: *const ir_node) -> Node {
        Node::Block(BlockNode(ir_node))
    }
    fn create_builtin(ir_node: *const ir_node) -> Node {
        Node::Builtin(BuiltinNode(ir_node))
    }
    fn create_call(ir_node: *const ir_node) -> Node {
        Node::Call(CallNode(ir_node))
    }
    fn create_cmp(ir_node: *const ir_node) -> Node {
        Node::Cmp(CmpNode(ir_node))
    }
    fn create_cond(ir_node: *const ir_node) -> Node {
        Node::Cond(CondNode(ir_node))
    }
    fn create_confirm(ir_node: *const ir_node) -> Node {
        Node::Confirm(ConfirmNode(ir_node))
    }
    fn create_const(ir_node: *const ir_node) -> Node {
        Node::Const(ConstNode(ir_node))
    }
    fn create_conv(ir_node: *const ir_node) -> Node {
        Node::Conv(ConvNode(ir_node))
    }
    fn create_copyb(ir_node: *const ir_node) -> Node {
        Node::CopyB(CopyBNode(ir_node))
    }
    fn create_deleted(ir_node: *const ir_node) -> Node {
        Node::Deleted(DeletedNode(ir_node))
    }
    fn create_div(ir_node: *const ir_node) -> Node {
        Node::Div(DivNode(ir_node))
    }
    fn create_dummy(ir_node: *const ir_node) -> Node {
        Node::Dummy(DummyNode(ir_node))
    }
    fn create_end(ir_node: *const ir_node) -> Node {
        Node::End(EndNode(ir_node))
    }
    fn create_eor(ir_node: *const ir_node) -> Node {
        Node::Eor(EorNode(ir_node))
    }
    fn create_free(ir_node: *const ir_node) -> Node {
        Node::Free(FreeNode(ir_node))
    }
    fn create_ijmp(ir_node: *const ir_node) -> Node {
        Node::IJmp(IJmpNode(ir_node))
    }
    fn create_id(ir_node: *const ir_node) -> Node {
        Node::Id(IdNode(ir_node))
    }
    fn create_jmp(ir_node: *const ir_node) -> Node {
        Node::Jmp(JmpNode(ir_node))
    }
    fn create_load(ir_node: *const ir_node) -> Node {
        Node::Load(LoadNode(ir_node))
    }
    fn create_member(ir_node: *const ir_node) -> Node {
        Node::Member(MemberNode(ir_node))
    }
    fn create_minus(ir_node: *const ir_node) -> Node {
        Node::Minus(MinusNode(ir_node))
    }
    fn create_mod(ir_node: *const ir_node) -> Node {
        Node::Mod(ModNode(ir_node))
    }
    fn create_mul(ir_node: *const ir_node) -> Node {
        Node::Mul(MulNode(ir_node))
    }
    fn create_mulh(ir_node: *const ir_node) -> Node {
        Node::Mulh(MulhNode(ir_node))
    }
    fn create_mux(ir_node: *const ir_node) -> Node {
        Node::Mux(MuxNode(ir_node))
    }
    fn create_nomem(ir_node: *const ir_node) -> Node {
        Node::NoMem(NoMemNode(ir_node))
    }
    fn create_not(ir_node: *const ir_node) -> Node {
        Node::Not(NotNode(ir_node))
    }
    fn create_offset(ir_node: *const ir_node) -> Node {
        Node::Offset(OffsetNode(ir_node))
    }
    fn create_or(ir_node: *const ir_node) -> Node {
        Node::Or(OrNode(ir_node))
    }
    fn create_phi(ir_node: *const ir_node) -> Node {
        Node::Phi(PhiNode(ir_node))
    }
    fn create_pin(ir_node: *const ir_node) -> Node {
        Node::Pin(PinNode(ir_node))
    }
    fn create_proj(ir_node: *const ir_node) -> Node {
        Node::Proj(ProjNode(ir_node))
    }
    fn create_raise(ir_node: *const ir_node) -> Node {
        Node::Raise(RaiseNode(ir_node))
    }
    fn create_return(ir_node: *const ir_node) -> Node {
        Node::Return(ReturnNode(ir_node))
    }
    fn create_sel(ir_node: *const ir_node) -> Node {
        Node::Sel(SelNode(ir_node))
    }
    fn create_shl(ir_node: *const ir_node) -> Node {
        Node::Shl(ShlNode(ir_node))
    }
    fn create_shr(ir_node: *const ir_node) -> Node {
        Node::Shr(ShrNode(ir_node))
    }
    fn create_shrs(ir_node: *const ir_node) -> Node {
        Node::Shrs(ShrsNode(ir_node))
    }
    fn create_size(ir_node: *const ir_node) -> Node {
        Node::Size(SizeNode(ir_node))
    }
    fn create_start(ir_node: *const ir_node) -> Node {
        Node::Start(StartNode(ir_node))
    }
    fn create_store(ir_node: *const ir_node) -> Node {
        Node::Store(StoreNode(ir_node))
    }
    fn create_sub(ir_node: *const ir_node) -> Node {
        Node::Sub(SubNode(ir_node))
    }
    fn create_switch(ir_node: *const ir_node) -> Node {
        Node::Switch(SwitchNode(ir_node))
    }
    fn create_sync(ir_node: *const ir_node) -> Node {
        Node::Sync(SyncNode(ir_node))
    }
    fn create_tuple(ir_node: *const ir_node) -> Node {
        Node::Tuple(TupleNode(ir_node))
    }
    fn create_unknown(ir_node: *const ir_node) -> Node {
        Node::Unknown(UnknownNode(ir_node))
    }
}

/// executes assembler fragments of the target machine.
/// 
/// The node contains a template for an assembler snippet. The compiler will
/// replace occurrences of %0 to %9 with input/output registers,
/// %% with a single % char. Some backends allow additional specifiers (for
/// example %w3, %l3, %h3 on x86 to get a 16bit, 8hit low, 8bit high part
/// of a register).
/// After the replacements the text is emitted into the final assembly.
/// 
/// The clobber list contains names of registers which have an undefined value
/// after the assembler instruction is executed; it may also contain 'memory'
/// or 'cc' if global state/memory changes or the condition code registers
/// (some backends implicitly set cc, memory clobbers on all ASM statements).
/// 
/// Example (an i386 instruction)::
/// 
/// ASM(text="btsl %1, %0",
/// constraints = ["=m", "r"],
/// clobbers = ["cc"])
/// 
/// As there are no output, the %0 references the first input which is just an
/// address which the asm operation writes to. %1 references to an input which
/// is passed as a register. The condition code register has an unknown value
/// after the instruction.
/// 
/// (This format is inspired by the gcc extended asm syntax)
#[derive(Debug)]
pub struct ASMNode(*const ir_node);

impl ASMNode {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let ir_node = bindings::get_ASM_mem(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_ASM_mem(self.0, ir_node)
    }

    /// Gets number of constraints.
    pub fn n_constraints(&self) -> Node {
        let ir_node = bindings::get_ASM_n_constraints(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets number of constraints.
    pub fn set_n_constraints(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_ASM_n_constraints(self.0, ir_node)
    }

    /// Gets constraints.
    pub fn constraints(&self) -> Node {
        let ir_node = bindings::get_ASM_constraints(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets constraints.
    pub fn set_constraints(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_ASM_constraints(self.0, ir_node)
    }

    /// Gets number of clobbered registers/memory.
    pub fn n_clobbers(&self) -> Node {
        let ir_node = bindings::get_ASM_n_clobbers(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets number of clobbered registers/memory.
    pub fn set_n_clobbers(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_ASM_n_clobbers(self.0, ir_node)
    }

    /// Gets list of clobbered registers/memory.
    pub fn clobbers(&self) -> Node {
        let ir_node = bindings::get_ASM_clobbers(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets list of clobbered registers/memory.
    pub fn set_clobbers(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_ASM_clobbers(self.0, ir_node)
    }

    /// Gets assembler text.
    pub fn text(&self) -> Node {
        let ir_node = bindings::get_ASM_text(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets assembler text.
    pub fn set_text(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_ASM_text(self.0, ir_node)
    }

}
impl Into<Node> for ASMNode {
    fn into(self) -> Node {
        Node::ASM(self)
    }
}

impl Into<*const ir_node> for ASMNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// returns the sum of its operands
#[derive(Debug)]
pub struct AddNode(*const ir_node);

impl AddNode {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let ir_node = bindings::get_Add_left(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets first operand.
    pub fn set_left(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Add_left(self.0, ir_node)
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let ir_node = bindings::get_Add_right(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets second operand.
    pub fn set_right(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Add_right(self.0, ir_node)
    }

}
impl Into<Node> for AddNode {
    fn into(self) -> Node {
        Node::Add(self)
    }
}

impl Into<*const ir_node> for AddNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Symbolic constant that represents the address of an entity (variable or
/// method)
#[derive(Debug)]
pub struct AddressNode(*const ir_node);

impl AddressNode {
}
impl Into<Node> for AddressNode {
    fn into(self) -> Node {
        Node::Address(self)
    }
}

impl Into<*const ir_node> for AddressNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// A symbolic constant that represents the alignment of a type
#[derive(Debug)]
pub struct AlignNode(*const ir_node);

impl AlignNode {
}
impl Into<Node> for AlignNode {
    fn into(self) -> Node {
        Node::Align(self)
    }
}

impl Into<*const ir_node> for AlignNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Allocates a block of memory on the stack.
#[derive(Debug)]
pub struct AllocNode(*const ir_node);

impl AllocNode {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let ir_node = bindings::get_Alloc_mem(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Alloc_mem(self.0, ir_node)
    }

    /// Gets size of the block in bytes.
    pub fn size(&self) -> Node {
        let ir_node = bindings::get_Alloc_size(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets size of the block in bytes.
    pub fn set_size(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Alloc_size(self.0, ir_node)
    }

    /// Gets alignment of the memory block (must be a power of 2).
    pub fn alignment(&self) -> Node {
        let ir_node = bindings::get_Alloc_alignment(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets alignment of the memory block (must be a power of 2).
    pub fn set_alignment(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Alloc_alignment(self.0, ir_node)
    }

}
impl Into<Node> for AllocNode {
    fn into(self) -> Node {
        Node::Alloc(self)
    }
}

impl Into<*const ir_node> for AllocNode {
    fn into(self) -> *const ir_node {
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
pub struct AnchorNode(*const ir_node);

impl AnchorNode {
    /// Gets block the end node belongs to.
    pub fn end_block(&self) -> Node {
        let ir_node = bindings::get_Anchor_end_block(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets block the end node belongs to.
    pub fn set_end_block(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Anchor_end_block(self.0, ir_node)
    }

    /// Gets block the start node belongs to.
    pub fn start_block(&self) -> Node {
        let ir_node = bindings::get_Anchor_start_block(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets block the start node belongs to.
    pub fn set_start_block(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Anchor_start_block(self.0, ir_node)
    }

    /// Gets end node of this ir_graph.
    pub fn end(&self) -> Node {
        let ir_node = bindings::get_Anchor_end(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets end node of this ir_graph.
    pub fn set_end(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Anchor_end(self.0, ir_node)
    }

    /// Gets start node of this ir_graph.
    pub fn start(&self) -> Node {
        let ir_node = bindings::get_Anchor_start(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets start node of this ir_graph.
    pub fn set_start(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Anchor_start(self.0, ir_node)
    }

    /// Gets frame of this ir_graph.
    pub fn frame(&self) -> Node {
        let ir_node = bindings::get_Anchor_frame(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets frame of this ir_graph.
    pub fn set_frame(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Anchor_frame(self.0, ir_node)
    }

    /// Gets initial memory of this ir_graph.
    pub fn initial_mem(&self) -> Node {
        let ir_node = bindings::get_Anchor_initial_mem(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets initial memory of this ir_graph.
    pub fn set_initial_mem(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Anchor_initial_mem(self.0, ir_node)
    }

    /// Gets argument proj of the start node.
    pub fn args(&self) -> Node {
        let ir_node = bindings::get_Anchor_args(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets argument proj of the start node.
    pub fn set_args(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Anchor_args(self.0, ir_node)
    }

    /// Gets the only NoMem node of this ir_graph.
    pub fn no_mem(&self) -> Node {
        let ir_node = bindings::get_Anchor_no_mem(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets the only NoMem node of this ir_graph.
    pub fn set_no_mem(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Anchor_no_mem(self.0, ir_node)
    }

}
impl Into<Node> for AnchorNode {
    fn into(self) -> Node {
        Node::Anchor(self)
    }
}

impl Into<*const ir_node> for AnchorNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// returns the result of a bitwise and operation of its operands
#[derive(Debug)]
pub struct AndNode(*const ir_node);

impl AndNode {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let ir_node = bindings::get_And_left(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets first operand.
    pub fn set_left(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_And_left(self.0, ir_node)
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let ir_node = bindings::get_And_right(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets second operand.
    pub fn set_right(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_And_right(self.0, ir_node)
    }

}
impl Into<Node> for AndNode {
    fn into(self) -> Node {
        Node::And(self)
    }
}

impl Into<*const ir_node> for AndNode {
    fn into(self) -> *const ir_node {
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
pub struct BadNode(*const ir_node);

impl BadNode {
}
impl Into<Node> for BadNode {
    fn into(self) -> Node {
        Node::Bad(self)
    }
}

impl Into<*const ir_node> for BadNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Converts a value between modes with different arithmetics but same
/// number of bits by reinterpreting the bits in the new mode
#[derive(Debug)]
pub struct BitcastNode(*const ir_node);

impl BitcastNode {
    /// Gets operand.
    pub fn op(&self) -> Node {
        let ir_node = bindings::get_Bitcast_op(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets operand.
    pub fn set_op(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Bitcast_op(self.0, ir_node)
    }

}
impl Into<Node> for BitcastNode {
    fn into(self) -> Node {
        Node::Bitcast(self)
    }
}

impl Into<*const ir_node> for BitcastNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// A basic block
#[derive(Debug)]
pub struct BlockNode(*const ir_node);

impl BlockNode {
    /// Gets entity representing this block.
    pub fn entity(&self) -> Node {
        let ir_node = bindings::get_Block_entity(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets entity representing this block.
    pub fn set_entity(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Block_entity(self.0, ir_node)
    }

}
impl Into<Node> for BlockNode {
    fn into(self) -> Node {
        Node::Block(self)
    }
}

impl Into<*const ir_node> for BlockNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// performs a backend-specific builtin.
#[derive(Debug)]
pub struct BuiltinNode(*const ir_node);

impl BuiltinNode {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let ir_node = bindings::get_Builtin_mem(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Builtin_mem(self.0, ir_node)
    }

    /// Gets kind of builtin.
    pub fn kind(&self) -> Node {
        let ir_node = bindings::get_Builtin_kind(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets kind of builtin.
    pub fn set_kind(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Builtin_kind(self.0, ir_node)
    }

    /// Gets method type for the builtin call.
    pub fn ty(&self) -> Node {
        let ir_node = bindings::get_Builtin_type(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets method type for the builtin call.
    pub fn set_type(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Builtin_type(self.0, ir_node)
    }

}
impl Into<Node> for BuiltinNode {
    fn into(self) -> Node {
        Node::Builtin(self)
    }
}

impl Into<*const ir_node> for BuiltinNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Calls other code. Control flow is transferred to ptr, additional
/// operands are passed to the called code. Called code usually performs a
/// return operation. The operands of this return operation are the result
/// of the Call node.
#[derive(Debug)]
pub struct CallNode(*const ir_node);

impl CallNode {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let ir_node = bindings::get_Call_mem(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Call_mem(self.0, ir_node)
    }

    /// Gets pointer to called code.
    pub fn ptr(&self) -> Node {
        let ir_node = bindings::get_Call_ptr(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets pointer to called code.
    pub fn set_ptr(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Call_ptr(self.0, ir_node)
    }

    /// Gets type of the call (usually type of the called procedure).
    pub fn ty(&self) -> Node {
        let ir_node = bindings::get_Call_type(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets type of the call (usually type of the called procedure).
    pub fn set_type(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Call_type(self.0, ir_node)
    }

}
impl Into<Node> for CallNode {
    fn into(self) -> Node {
        Node::Call(self)
    }
}

impl Into<*const ir_node> for CallNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Compares its two operands and checks whether a specified
/// relation (like less or equal) is fulfilled.
#[derive(Debug)]
pub struct CmpNode(*const ir_node);

impl CmpNode {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let ir_node = bindings::get_Cmp_left(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets first operand.
    pub fn set_left(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Cmp_left(self.0, ir_node)
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let ir_node = bindings::get_Cmp_right(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets second operand.
    pub fn set_right(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Cmp_right(self.0, ir_node)
    }

    /// Gets Comparison relation.
    pub fn relation(&self) -> Node {
        let ir_node = bindings::get_Cmp_relation(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets Comparison relation.
    pub fn set_relation(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Cmp_relation(self.0, ir_node)
    }

}
impl Into<Node> for CmpNode {
    fn into(self) -> Node {
        Node::Cmp(self)
    }
}

impl Into<*const ir_node> for CmpNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Conditionally change control flow.
#[derive(Debug)]
pub struct CondNode(*const ir_node);

impl CondNode {
    /// Gets condition parameter.
    pub fn selector(&self) -> Node {
        let ir_node = bindings::get_Cond_selector(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets condition parameter.
    pub fn set_selector(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Cond_selector(self.0, ir_node)
    }

    /// Gets can indicate the most likely jump.
    pub fn jmp_pred(&self) -> Node {
        let ir_node = bindings::get_Cond_jmp_pred(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets can indicate the most likely jump.
    pub fn set_jmp_pred(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Cond_jmp_pred(self.0, ir_node)
    }

}
impl Into<Node> for CondNode {
    fn into(self) -> Node {
        Node::Cond(self)
    }
}

impl Into<*const ir_node> for CondNode {
    fn into(self) -> *const ir_node {
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
pub struct ConfirmNode(*const ir_node);

impl ConfirmNode {
    /// Gets value to express a constraint for.
    pub fn value(&self) -> Node {
        let ir_node = bindings::get_Confirm_value(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets value to express a constraint for.
    pub fn set_value(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Confirm_value(self.0, ir_node)
    }

    /// Gets value to compare against.
    pub fn bound(&self) -> Node {
        let ir_node = bindings::get_Confirm_bound(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets value to compare against.
    pub fn set_bound(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Confirm_bound(self.0, ir_node)
    }

    /// Gets relation of value to bound.
    pub fn relation(&self) -> Node {
        let ir_node = bindings::get_Confirm_relation(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets relation of value to bound.
    pub fn set_relation(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Confirm_relation(self.0, ir_node)
    }

}
impl Into<Node> for ConfirmNode {
    fn into(self) -> Node {
        Node::Confirm(self)
    }
}

impl Into<*const ir_node> for ConfirmNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Returns a constant value.
#[derive(Debug)]
pub struct ConstNode(*const ir_node);

impl ConstNode {
    /// Gets constant value (a tarval object).
    pub fn tarval(&self) -> Node {
        let ir_node = bindings::get_Const_tarval(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets constant value (a tarval object).
    pub fn set_tarval(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Const_tarval(self.0, ir_node)
    }

}
impl Into<Node> for ConstNode {
    fn into(self) -> Node {
        Node::Const(self)
    }
}

impl Into<*const ir_node> for ConstNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Converts values between modes
#[derive(Debug)]
pub struct ConvNode(*const ir_node);

impl ConvNode {
    /// Gets operand.
    pub fn op(&self) -> Node {
        let ir_node = bindings::get_Conv_op(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets operand.
    pub fn set_op(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Conv_op(self.0, ir_node)
    }

}
impl Into<Node> for ConvNode {
    fn into(self) -> Node {
        Node::Conv(self)
    }
}

impl Into<*const ir_node> for ConvNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Copies a block of memory with statically known size/type.
#[derive(Debug)]
pub struct CopyBNode(*const ir_node);

impl CopyBNode {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let ir_node = bindings::get_CopyB_mem(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_CopyB_mem(self.0, ir_node)
    }

    /// Gets destination address.
    pub fn dst(&self) -> Node {
        let ir_node = bindings::get_CopyB_dst(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets destination address.
    pub fn set_dst(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_CopyB_dst(self.0, ir_node)
    }

    /// Gets source address.
    pub fn src(&self) -> Node {
        let ir_node = bindings::get_CopyB_src(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets source address.
    pub fn set_src(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_CopyB_src(self.0, ir_node)
    }

    /// Gets type of copied data.
    pub fn ty(&self) -> Node {
        let ir_node = bindings::get_CopyB_type(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets type of copied data.
    pub fn set_type(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_CopyB_type(self.0, ir_node)
    }

    /// Gets volatile CopyB nodes have a visible side-effect and may not be optimized.
    pub fn volatility(&self) -> Node {
        let ir_node = bindings::get_CopyB_volatility(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets volatile CopyB nodes have a visible side-effect and may not be optimized.
    pub fn set_volatility(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_CopyB_volatility(self.0, ir_node)
    }

}
impl Into<Node> for CopyBNode {
    fn into(self) -> Node {
        Node::CopyB(self)
    }
}

impl Into<*const ir_node> for CopyBNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Internal node which is temporary set to nodes which are already removed
/// from the graph.
#[derive(Debug)]
pub struct DeletedNode(*const ir_node);

impl DeletedNode {
}
impl Into<Node> for DeletedNode {
    fn into(self) -> Node {
        Node::Deleted(self)
    }
}

impl Into<*const ir_node> for DeletedNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// returns the quotient of its 2 operands
#[derive(Debug)]
pub struct DivNode(*const ir_node);

impl DivNode {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let ir_node = bindings::get_Div_mem(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Div_mem(self.0, ir_node)
    }

    /// Gets first operand.
    pub fn left(&self) -> Node {
        let ir_node = bindings::get_Div_left(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets first operand.
    pub fn set_left(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Div_left(self.0, ir_node)
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let ir_node = bindings::get_Div_right(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets second operand.
    pub fn set_right(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Div_right(self.0, ir_node)
    }

    /// Gets mode of the result value.
    pub fn resmode(&self) -> Node {
        let ir_node = bindings::get_Div_resmode(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets mode of the result value.
    pub fn set_resmode(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Div_resmode(self.0, ir_node)
    }

    /// Gets Set when division remainder is known to be zero.
    pub fn no_remainder(&self) -> Node {
        let ir_node = bindings::get_Div_no_remainder(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets Set when division remainder is known to be zero.
    pub fn set_no_remainder(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Div_no_remainder(self.0, ir_node)
    }

}
impl Into<Node> for DivNode {
    fn into(self) -> Node {
        Node::Div(self)
    }
}

impl Into<*const ir_node> for DivNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// A placeholder value. This is used when constructing cyclic graphs where
/// you have cases where not all predecessors of a phi-node are known. Dummy
/// nodes are used for the unknown predecessors and replaced later.
#[derive(Debug)]
pub struct DummyNode(*const ir_node);

impl DummyNode {
}
impl Into<Node> for DummyNode {
    fn into(self) -> Node {
        Node::Dummy(self)
    }
}

impl Into<*const ir_node> for DummyNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Last node of a graph. It references nodes in endless loops (so called
/// keepalive edges)
#[derive(Debug)]
pub struct EndNode(*const ir_node);

impl EndNode {
}
impl Into<Node> for EndNode {
    fn into(self) -> Node {
        Node::End(self)
    }
}

impl Into<*const ir_node> for EndNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// returns the result of a bitwise exclusive or operation of its operands.
/// 
/// This is also known as the Xor operation.
#[derive(Debug)]
pub struct EorNode(*const ir_node);

impl EorNode {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let ir_node = bindings::get_Eor_left(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets first operand.
    pub fn set_left(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Eor_left(self.0, ir_node)
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let ir_node = bindings::get_Eor_right(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets second operand.
    pub fn set_right(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Eor_right(self.0, ir_node)
    }

}
impl Into<Node> for EorNode {
    fn into(self) -> Node {
        Node::Eor(self)
    }
}

impl Into<*const ir_node> for EorNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Frees a block of memory previously allocated by an Alloc node
#[derive(Debug)]
pub struct FreeNode(*const ir_node);

impl FreeNode {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let ir_node = bindings::get_Free_mem(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Free_mem(self.0, ir_node)
    }

    /// Gets pointer to the object to free.
    pub fn ptr(&self) -> Node {
        let ir_node = bindings::get_Free_ptr(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets pointer to the object to free.
    pub fn set_ptr(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Free_ptr(self.0, ir_node)
    }

}
impl Into<Node> for FreeNode {
    fn into(self) -> Node {
        Node::Free(self)
    }
}

impl Into<*const ir_node> for FreeNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Jumps to the code in its argument. The code has to be in the same
/// function and the destination must be one of the blocks reachable
/// by the tuple results
#[derive(Debug)]
pub struct IJmpNode(*const ir_node);

impl IJmpNode {
    /// Gets target address of the jump.
    pub fn target(&self) -> Node {
        let ir_node = bindings::get_IJmp_target(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets target address of the jump.
    pub fn set_target(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_IJmp_target(self.0, ir_node)
    }

}
impl Into<Node> for IJmpNode {
    fn into(self) -> Node {
        Node::IJmp(self)
    }
}

impl Into<*const ir_node> for IJmpNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Returns its operand unchanged.
/// 
/// This is mainly used when exchanging nodes. Usually you shouldn't see Id
/// nodes since the getters/setters for node inputs skip them automatically.
#[derive(Debug)]
pub struct IdNode(*const ir_node);

impl IdNode {
    /// Gets the value which is returned unchanged.
    pub fn pred(&self) -> Node {
        let ir_node = bindings::get_Id_pred(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets the value which is returned unchanged.
    pub fn set_pred(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Id_pred(self.0, ir_node)
    }

}
impl Into<Node> for IdNode {
    fn into(self) -> Node {
        Node::Id(self)
    }
}

impl Into<*const ir_node> for IdNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Jumps to the block connected through the out-value
#[derive(Debug)]
pub struct JmpNode(*const ir_node);

impl JmpNode {
}
impl Into<Node> for JmpNode {
    fn into(self) -> Node {
        Node::Jmp(self)
    }
}

impl Into<*const ir_node> for JmpNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Loads a value from memory (heap or stack).
#[derive(Debug)]
pub struct LoadNode(*const ir_node);

impl LoadNode {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let ir_node = bindings::get_Load_mem(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Load_mem(self.0, ir_node)
    }

    /// Gets address to load from.
    pub fn ptr(&self) -> Node {
        let ir_node = bindings::get_Load_ptr(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets address to load from.
    pub fn set_ptr(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Load_ptr(self.0, ir_node)
    }

    /// Gets mode of the value to be loaded.
    pub fn mode(&self) -> Node {
        let ir_node = bindings::get_Load_mode(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets mode of the value to be loaded.
    pub fn set_mode(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Load_mode(self.0, ir_node)
    }

    /// Gets The type of the object which is stored at ptr (need not match with mode).
    pub fn ty(&self) -> Node {
        let ir_node = bindings::get_Load_type(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets The type of the object which is stored at ptr (need not match with mode).
    pub fn set_type(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Load_type(self.0, ir_node)
    }

    /// Gets volatile loads are a visible side-effect and may not be optimized.
    pub fn volatility(&self) -> Node {
        let ir_node = bindings::get_Load_volatility(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets volatile loads are a visible side-effect and may not be optimized.
    pub fn set_volatility(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Load_volatility(self.0, ir_node)
    }

    /// Gets pointers to unaligned loads don't need to respect the load-mode/type alignments.
    pub fn unaligned(&self) -> Node {
        let ir_node = bindings::get_Load_unaligned(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets pointers to unaligned loads don't need to respect the load-mode/type alignments.
    pub fn set_unaligned(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Load_unaligned(self.0, ir_node)
    }

}
impl Into<Node> for LoadNode {
    fn into(self) -> Node {
        Node::Load(self)
    }
}

impl Into<*const ir_node> for LoadNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Computes the address of a compound type member given the base address
/// of an instance of the compound type.
/// 
/// A Member node must only produce a NULL pointer if the ptr input is NULL.
#[derive(Debug)]
pub struct MemberNode(*const ir_node);

impl MemberNode {
    /// Gets pointer to object to select from.
    pub fn ptr(&self) -> Node {
        let ir_node = bindings::get_Member_ptr(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets pointer to object to select from.
    pub fn set_ptr(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Member_ptr(self.0, ir_node)
    }

    /// Gets entity which is selected.
    pub fn entity(&self) -> Node {
        let ir_node = bindings::get_Member_entity(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets entity which is selected.
    pub fn set_entity(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Member_entity(self.0, ir_node)
    }

}
impl Into<Node> for MemberNode {
    fn into(self) -> Node {
        Node::Member(self)
    }
}

impl Into<*const ir_node> for MemberNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// returns the additive inverse of its operand
#[derive(Debug)]
pub struct MinusNode(*const ir_node);

impl MinusNode {
    /// Gets operand.
    pub fn op(&self) -> Node {
        let ir_node = bindings::get_Minus_op(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets operand.
    pub fn set_op(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Minus_op(self.0, ir_node)
    }

}
impl Into<Node> for MinusNode {
    fn into(self) -> Node {
        Node::Minus(self)
    }
}

impl Into<*const ir_node> for MinusNode {
    fn into(self) -> *const ir_node {
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
pub struct ModNode(*const ir_node);

impl ModNode {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let ir_node = bindings::get_Mod_mem(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Mod_mem(self.0, ir_node)
    }

    /// Gets first operand.
    pub fn left(&self) -> Node {
        let ir_node = bindings::get_Mod_left(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets first operand.
    pub fn set_left(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Mod_left(self.0, ir_node)
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let ir_node = bindings::get_Mod_right(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets second operand.
    pub fn set_right(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Mod_right(self.0, ir_node)
    }

    /// Gets mode of the result.
    pub fn resmode(&self) -> Node {
        let ir_node = bindings::get_Mod_resmode(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets mode of the result.
    pub fn set_resmode(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Mod_resmode(self.0, ir_node)
    }

}
impl Into<Node> for ModNode {
    fn into(self) -> Node {
        Node::Mod(self)
    }
}

impl Into<*const ir_node> for ModNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// returns the product of its operands
#[derive(Debug)]
pub struct MulNode(*const ir_node);

impl MulNode {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let ir_node = bindings::get_Mul_left(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets first operand.
    pub fn set_left(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Mul_left(self.0, ir_node)
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let ir_node = bindings::get_Mul_right(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets second operand.
    pub fn set_right(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Mul_right(self.0, ir_node)
    }

}
impl Into<Node> for MulNode {
    fn into(self) -> Node {
        Node::Mul(self)
    }
}

impl Into<*const ir_node> for MulNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// returns the upper word of the product of its operands (the part which
/// would not fit into the result mode of a normal Mul anymore)
#[derive(Debug)]
pub struct MulhNode(*const ir_node);

impl MulhNode {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let ir_node = bindings::get_Mulh_left(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets first operand.
    pub fn set_left(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Mulh_left(self.0, ir_node)
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let ir_node = bindings::get_Mulh_right(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets second operand.
    pub fn set_right(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Mulh_right(self.0, ir_node)
    }

}
impl Into<Node> for MulhNode {
    fn into(self) -> Node {
        Node::Mulh(self)
    }
}

impl Into<*const ir_node> for MulhNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// returns the false or true operand depending on the value of the sel
/// operand
#[derive(Debug)]
pub struct MuxNode(*const ir_node);

impl MuxNode {
    /// Gets value making the output selection.
    pub fn sel(&self) -> Node {
        let ir_node = bindings::get_Mux_sel(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets value making the output selection.
    pub fn set_sel(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Mux_sel(self.0, ir_node)
    }

    /// Gets selected if sel input is false.
    pub fn false(&self) -> Node {
        let ir_node = bindings::get_Mux_false(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets selected if sel input is false.
    pub fn set_false(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Mux_false(self.0, ir_node)
    }

    /// Gets selected if sel input is true.
    pub fn true(&self) -> Node {
        let ir_node = bindings::get_Mux_true(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets selected if sel input is true.
    pub fn set_true(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Mux_true(self.0, ir_node)
    }

}
impl Into<Node> for MuxNode {
    fn into(self) -> Node {
        Node::Mux(self)
    }
}

impl Into<*const ir_node> for MuxNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Placeholder node for cases where you don't need any memory input
#[derive(Debug)]
pub struct NoMemNode(*const ir_node);

impl NoMemNode {
}
impl Into<Node> for NoMemNode {
    fn into(self) -> Node {
        Node::NoMem(self)
    }
}

impl Into<*const ir_node> for NoMemNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// returns the bitwise complement of a value. Works for boolean values, too.
#[derive(Debug)]
pub struct NotNode(*const ir_node);

impl NotNode {
    /// Gets operand.
    pub fn op(&self) -> Node {
        let ir_node = bindings::get_Not_op(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets operand.
    pub fn set_op(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Not_op(self.0, ir_node)
    }

}
impl Into<Node> for NotNode {
    fn into(self) -> Node {
        Node::Not(self)
    }
}

impl Into<*const ir_node> for NotNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Symbolic constant that represents the offset of an entity in its owner type.
#[derive(Debug)]
pub struct OffsetNode(*const ir_node);

impl OffsetNode {
}
impl Into<Node> for OffsetNode {
    fn into(self) -> Node {
        Node::Offset(self)
    }
}

impl Into<*const ir_node> for OffsetNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// returns the result of a bitwise or operation of its operands
#[derive(Debug)]
pub struct OrNode(*const ir_node);

impl OrNode {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let ir_node = bindings::get_Or_left(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets first operand.
    pub fn set_left(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Or_left(self.0, ir_node)
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let ir_node = bindings::get_Or_right(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets second operand.
    pub fn set_right(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Or_right(self.0, ir_node)
    }

}
impl Into<Node> for OrNode {
    fn into(self) -> Node {
        Node::Or(self)
    }
}

impl Into<*const ir_node> for OrNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Choose a value based on control flow. A phi node has 1 input for each
/// predecessor of its block. If a block is entered from its nth predecessor
/// all phi nodes produce their nth input as result.
#[derive(Debug)]
pub struct PhiNode(*const ir_node);

impl PhiNode {
    /// Gets whether Phi represents the observable effect of a (possibly) nonterminating loop.
    pub fn loop(&self) -> Node {
        let ir_node = bindings::get_Phi_loop(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets whether Phi represents the observable effect of a (possibly) nonterminating loop.
    pub fn set_loop(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Phi_loop(self.0, ir_node)
    }

}
impl Into<Node> for PhiNode {
    fn into(self) -> Node {
        Node::Phi(self)
    }
}

impl Into<*const ir_node> for PhiNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Pin the value of the node node in the current block. No users of the Pin
/// node can float above the Block of the Pin. The node cannot float behind
/// this block. Often used to Pin the NoMem node.
#[derive(Debug)]
pub struct PinNode(*const ir_node);

impl PinNode {
    /// Gets value which is pinned.
    pub fn op(&self) -> Node {
        let ir_node = bindings::get_Pin_op(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets value which is pinned.
    pub fn set_op(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Pin_op(self.0, ir_node)
    }

}
impl Into<Node> for PinNode {
    fn into(self) -> Node {
        Node::Pin(self)
    }
}

impl Into<*const ir_node> for PinNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// returns an entry of a tuple value
#[derive(Debug)]
pub struct ProjNode(*const ir_node);

impl ProjNode {
    /// Gets the tuple value from which a part is extracted.
    pub fn pred(&self) -> Node {
        let ir_node = bindings::get_Proj_pred(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets the tuple value from which a part is extracted.
    pub fn set_pred(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Proj_pred(self.0, ir_node)
    }

    /// Gets number of tuple component to be extracted.
    pub fn num(&self) -> Node {
        let ir_node = bindings::get_Proj_num(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets number of tuple component to be extracted.
    pub fn set_num(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Proj_num(self.0, ir_node)
    }

}
impl Into<Node> for ProjNode {
    fn into(self) -> Node {
        Node::Proj(self)
    }
}

impl Into<*const ir_node> for ProjNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Raises an exception. Unconditional change of control flow. Writes an
/// explicit Except variable to memory to pass it to the exception handler.
/// Must be lowered to a Call to a runtime check function.
#[derive(Debug)]
pub struct RaiseNode(*const ir_node);

impl RaiseNode {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let ir_node = bindings::get_Raise_mem(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Raise_mem(self.0, ir_node)
    }

    /// Gets pointer to exception object to be thrown.
    pub fn exo_ptr(&self) -> Node {
        let ir_node = bindings::get_Raise_exo_ptr(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets pointer to exception object to be thrown.
    pub fn set_exo_ptr(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Raise_exo_ptr(self.0, ir_node)
    }

}
impl Into<Node> for RaiseNode {
    fn into(self) -> Node {
        Node::Raise(self)
    }
}

impl Into<*const ir_node> for RaiseNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Returns from the current function. Takes memory and return values as
/// operands.
#[derive(Debug)]
pub struct ReturnNode(*const ir_node);

impl ReturnNode {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let ir_node = bindings::get_Return_mem(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Return_mem(self.0, ir_node)
    }

}
impl Into<Node> for ReturnNode {
    fn into(self) -> Node {
        Node::Return(self)
    }
}

impl Into<*const ir_node> for ReturnNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Computes the address of an array element from the array base pointer and
/// an index.
/// 
/// A Sel node must only produce a NULL pointer if the ptr input is NULL.
#[derive(Debug)]
pub struct SelNode(*const ir_node);

impl SelNode {
    /// Gets pointer to array to select from.
    pub fn ptr(&self) -> Node {
        let ir_node = bindings::get_Sel_ptr(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets pointer to array to select from.
    pub fn set_ptr(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Sel_ptr(self.0, ir_node)
    }

    /// Gets index to select.
    pub fn index(&self) -> Node {
        let ir_node = bindings::get_Sel_index(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets index to select.
    pub fn set_index(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Sel_index(self.0, ir_node)
    }

    /// Gets array type.
    pub fn ty(&self) -> Node {
        let ir_node = bindings::get_Sel_type(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets array type.
    pub fn set_type(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Sel_type(self.0, ir_node)
    }

}
impl Into<Node> for SelNode {
    fn into(self) -> Node {
        Node::Sel(self)
    }
}

impl Into<*const ir_node> for SelNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Returns its first operands bits shifted left by the amount of the 2nd
/// operand.
/// The right input (shift amount) must be an unsigned integer value.
/// If the result mode has modulo_shift!=0, then the effective shift amount is
/// the right input modulo this modulo_shift amount.
#[derive(Debug)]
pub struct ShlNode(*const ir_node);

impl ShlNode {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let ir_node = bindings::get_Shl_left(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets first operand.
    pub fn set_left(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Shl_left(self.0, ir_node)
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let ir_node = bindings::get_Shl_right(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets second operand.
    pub fn set_right(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Shl_right(self.0, ir_node)
    }

}
impl Into<Node> for ShlNode {
    fn into(self) -> Node {
        Node::Shl(self)
    }
}

impl Into<*const ir_node> for ShlNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Returns its first operands bits shifted right by the amount of the 2nd
/// operand. No special handling for the sign bit is performed (zero extension).
/// The right input (shift amount) must be an unsigned integer value.
/// If the result mode has modulo_shift!=0, then the effective shift amount is
/// the right input modulo this modulo_shift amount.
#[derive(Debug)]
pub struct ShrNode(*const ir_node);

impl ShrNode {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let ir_node = bindings::get_Shr_left(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets first operand.
    pub fn set_left(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Shr_left(self.0, ir_node)
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let ir_node = bindings::get_Shr_right(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets second operand.
    pub fn set_right(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Shr_right(self.0, ir_node)
    }

}
impl Into<Node> for ShrNode {
    fn into(self) -> Node {
        Node::Shr(self)
    }
}

impl Into<*const ir_node> for ShrNode {
    fn into(self) -> *const ir_node {
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
pub struct ShrsNode(*const ir_node);

impl ShrsNode {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let ir_node = bindings::get_Shrs_left(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets first operand.
    pub fn set_left(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Shrs_left(self.0, ir_node)
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let ir_node = bindings::get_Shrs_right(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets second operand.
    pub fn set_right(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Shrs_right(self.0, ir_node)
    }

}
impl Into<Node> for ShrsNode {
    fn into(self) -> Node {
        Node::Shrs(self)
    }
}

impl Into<*const ir_node> for ShrsNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// A symbolic constant that represents the size of a type
#[derive(Debug)]
pub struct SizeNode(*const ir_node);

impl SizeNode {
}
impl Into<Node> for SizeNode {
    fn into(self) -> Node {
        Node::Size(self)
    }
}

impl Into<*const ir_node> for SizeNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// The first node of a graph. Execution starts with this node.
#[derive(Debug)]
pub struct StartNode(*const ir_node);

impl StartNode {
}
impl Into<Node> for StartNode {
    fn into(self) -> Node {
        Node::Start(self)
    }
}

impl Into<*const ir_node> for StartNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Stores a value into memory (heap or stack).
#[derive(Debug)]
pub struct StoreNode(*const ir_node);

impl StoreNode {
    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let ir_node = bindings::get_Store_mem(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Store_mem(self.0, ir_node)
    }

    /// Gets address to store to.
    pub fn ptr(&self) -> Node {
        let ir_node = bindings::get_Store_ptr(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets address to store to.
    pub fn set_ptr(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Store_ptr(self.0, ir_node)
    }

    /// Gets value to store.
    pub fn value(&self) -> Node {
        let ir_node = bindings::get_Store_value(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets value to store.
    pub fn set_value(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Store_value(self.0, ir_node)
    }

    /// Gets The type of the object which is stored at ptr (need not match with value's type).
    pub fn ty(&self) -> Node {
        let ir_node = bindings::get_Store_type(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets The type of the object which is stored at ptr (need not match with value's type).
    pub fn set_type(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Store_type(self.0, ir_node)
    }

    /// Gets volatile stores are a visible side-effect and may not be optimized.
    pub fn volatility(&self) -> Node {
        let ir_node = bindings::get_Store_volatility(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets volatile stores are a visible side-effect and may not be optimized.
    pub fn set_volatility(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Store_volatility(self.0, ir_node)
    }

    /// Gets pointers to unaligned stores don't need to respect the load-mode/type alignments.
    pub fn unaligned(&self) -> Node {
        let ir_node = bindings::get_Store_unaligned(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets pointers to unaligned stores don't need to respect the load-mode/type alignments.
    pub fn set_unaligned(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Store_unaligned(self.0, ir_node)
    }

}
impl Into<Node> for StoreNode {
    fn into(self) -> Node {
        Node::Store(self)
    }
}

impl Into<*const ir_node> for StoreNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// returns the difference of its operands
#[derive(Debug)]
pub struct SubNode(*const ir_node);

impl SubNode {
    /// Gets first operand.
    pub fn left(&self) -> Node {
        let ir_node = bindings::get_Sub_left(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets first operand.
    pub fn set_left(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Sub_left(self.0, ir_node)
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let ir_node = bindings::get_Sub_right(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets second operand.
    pub fn set_right(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Sub_right(self.0, ir_node)
    }

}
impl Into<Node> for SubNode {
    fn into(self) -> Node {
        Node::Sub(self)
    }
}

impl Into<*const ir_node> for SubNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// Change control flow. The destination is chosen based on an integer
/// input value which is looked up in a table.
/// 
/// Backends can implement this efficiently using a jump table.
#[derive(Debug)]
pub struct SwitchNode(*const ir_node);

impl SwitchNode {
    /// Gets input selector.
    pub fn selector(&self) -> Node {
        let ir_node = bindings::get_Switch_selector(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets input selector.
    pub fn set_selector(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Switch_selector(self.0, ir_node)
    }

    /// Gets number of outputs (including pn_Switch_default).
    pub fn n_outs(&self) -> Node {
        let ir_node = bindings::get_Switch_n_outs(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets number of outputs (including pn_Switch_default).
    pub fn set_n_outs(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Switch_n_outs(self.0, ir_node)
    }

    /// Gets table describing mapping from input values to Proj numbers.
    pub fn table(&self) -> Node {
        let ir_node = bindings::get_Switch_table(self.0);
        NodeFactory::node(ir_node)
    }

    /// Sets table describing mapping from input values to Proj numbers.
    pub fn set_table(&self, node: &'_ Node) {
        let ir_node = node.into()
        bindings::set_Switch_table(self.0, ir_node)
    }

}
impl Into<Node> for SwitchNode {
    fn into(self) -> Node {
        Node::Switch(self)
    }
}

impl Into<*const ir_node> for SwitchNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

/// The Sync operation unifies several partial memory blocks. These blocks
/// have to be pairwise disjunct or the values in common locations have to
/// be identical.  This operation allows to specify all operations that
/// eventually need several partial memory blocks as input with a single
/// entrance by unifying the memories with a preceding Sync operation.
#[derive(Debug)]
pub struct SyncNode(*const ir_node);

impl SyncNode {
}
impl Into<Node> for SyncNode {
    fn into(self) -> Node {
        Node::Sync(self)
    }
}

impl Into<*const ir_node> for SyncNode {
    fn into(self) -> *const ir_node {
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
pub struct TupleNode(*const ir_node);

impl TupleNode {
}
impl Into<Node> for TupleNode {
    fn into(self) -> Node {
        Node::Tuple(self)
    }
}

impl Into<*const ir_node> for TupleNode {
    fn into(self) -> *const ir_node {
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
pub struct UnknownNode(*const ir_node);

impl UnknownNode {
}
impl Into<Node> for UnknownNode {
    fn into(self) -> Node {
        Node::Unknown(self)
    }
}

impl Into<*const ir_node> for UnknownNode {
    fn into(self) -> *const ir_node {
        self.0
    }
}

