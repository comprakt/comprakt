use super::{graph::Graph, nodes::NodeTrait};
use libfirm_rs_bindings as bindings;
use std::{collections::HashMap, fmt};
use strum_macros::EnumDiscriminants;

#[strum_discriminants(derive(Display))]
#[derive(EnumDiscriminants, Clone, Copy, Eq, PartialEq)]
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
    Proj(Proj, ProjKind),
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

impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::Add(node) => write!(f, "{:?}", node),
            Node::Address(node) => write!(f, "{:?}", node),
            Node::Align(node) => write!(f, "{:?}", node),
            Node::Alloc(node) => write!(f, "{:?}", node),
            Node::Anchor(node) => write!(f, "{:?}", node),
            Node::And(node) => write!(f, "{:?}", node),
            Node::Bad(node) => write!(f, "{:?}", node),
            Node::Bitcast(node) => write!(f, "{:?}", node),
            Node::Block(node) => write!(f, "{:?}", node),
            Node::Builtin(node) => write!(f, "{:?}", node),
            Node::Call(node) => write!(f, "{:?}", node),
            Node::Cmp(node) => write!(f, "{:?}", node),
            Node::Cond(node) => write!(f, "{:?}", node),
            Node::Confirm(node) => write!(f, "{:?}", node),
            Node::Const(node) => write!(f, "{:?}", node),
            Node::Conv(node) => write!(f, "{:?}", node),
            Node::CopyB(node) => write!(f, "{:?}", node),
            Node::Deleted(node) => write!(f, "{:?}", node),
            Node::Div(node) => write!(f, "{:?}", node),
            Node::Dummy(node) => write!(f, "{:?}", node),
            Node::End(node) => write!(f, "{:?}", node),
            Node::Eor(node) => write!(f, "{:?}", node),
            Node::Free(node) => write!(f, "{:?}", node),
            Node::IJmp(node) => write!(f, "{:?}", node),
            Node::Id(node) => write!(f, "{:?}", node),
            Node::Jmp(node) => write!(f, "{:?}", node),
            Node::Load(node) => write!(f, "{:?}", node),
            Node::Member(node) => write!(f, "{:?}", node),
            Node::Minus(node) => write!(f, "{:?}", node),
            Node::Mod(node) => write!(f, "{:?}", node),
            Node::Mul(node) => write!(f, "{:?}", node),
            Node::Mulh(node) => write!(f, "{:?}", node),
            Node::Mux(node) => write!(f, "{:?}", node),
            Node::NoMem(node) => write!(f, "{:?}", node),
            Node::Not(node) => write!(f, "{:?}", node),
            Node::Offset(node) => write!(f, "{:?}", node),
            Node::Or(node) => write!(f, "{:?}", node),
            Node::Phi(node) => write!(f, "{:?}", node),
            Node::Pin(node) => write!(f, "{:?}", node),
            Node::Proj(node, proj_kind) => write!(f, "{:?}: {:?}", node, proj_kind),
            Node::Raise(node) => write!(f, "{:?}", node),
            Node::Return(node) => write!(f, "{:?}", node),
            Node::Sel(node) => write!(f, "{:?}", node),
            Node::Shl(node) => write!(f, "{:?}", node),
            Node::Shr(node) => write!(f, "{:?}", node),
            Node::Shrs(node) => write!(f, "{:?}", node),
            Node::Size(node) => write!(f, "{:?}", node),
            Node::Start(node) => write!(f, "{:?}", node),
            Node::Store(node) => write!(f, "{:?}", node),
            Node::Sub(node) => write!(f, "{:?}", node),
            Node::Switch(node) => write!(f, "{:?}", node),
            Node::Sync(node) => write!(f, "{:?}", node),
            Node::Tuple(node) => write!(f, "{:?}", node),
            Node::Unknown(node) => write!(f, "{:?}", node),
        }
    }
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
            Node::Proj(node, _) => node.internal_ir_node(),
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

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ProjKind {
    /// memory result
    Alloc_M(Alloc),
    /// pointer to newly allocated memory
    Alloc_Res(Alloc),
    /// memory result
    Builtin_M(Builtin),
    /// memory result
    Call_M(Call),
    /// tuple containing all results
    Call_TResult(Call),
    /// control flow when no exception occurs
    Call_XRegular(Call),
    /// control flow when exception occurred
    Call_XExcept(Call),
    /// control flow if operand is "false"
    Cond_False(Cond),
    /// control flow if operand is "true"
    Cond_True(Cond),
    /// memory result
    Div_M(Div),
    /// result of computation
    Div_Res(Div),
    /// control flow when no exception occurs
    Div_XRegular(Div),
    /// control flow when exception occurred
    Div_XExcept(Div),
    /// memory result
    Load_M(Load),
    /// result of load operation
    Load_Res(Load),
    /// control flow when no exception occurs
    Load_XRegular(Load),
    /// control flow when exception occurred
    Load_XExcept(Load),
    /// memory result
    Mod_M(Mod),
    /// result of computation
    Mod_Res(Mod),
    /// control flow when no exception occurs
    Mod_XRegular(Mod),
    /// control flow when exception occurred
    Mod_XExcept(Mod),
    /// memory result
    Raise_M(Raise),
    /// control flow to exception handler
    Raise_X(Raise),
    /// initial memory
    Start_M(Start),
    /// frame base pointer
    Start_PFrameBase(Start),
    /// function arguments
    Start_TArgs(Start),
    Start_TArgs_Arg(
        /* arg_idx */ u32,
        /* pred_pred */ Start,
        /* pred */ Proj,
    ),
    /// memory result
    Store_M(Store),
    /// control flow when no exception occurs
    Store_XRegular(Store),
    /// control flow when exception occurred
    Store_XExcept(Store),
    /// control flow if no other case matches
    Switch_Default(Switch),
    Other,
}

type NodeFactoryFn = fn(*mut bindings::ir_node) -> Node;
pub struct NodeFactory(HashMap<u32, NodeFactoryFn>);
impl NodeFactory {
    pub fn new() -> Self {
        let mut map = HashMap::<u32, NodeFactoryFn>::new();
        unsafe {
            let op = bindings::get_op_Add();
            map.insert(bindings::get_op_code(op), Self::create_add);
            let op = bindings::get_op_Address();
            map.insert(bindings::get_op_code(op), Self::create_address);
            let op = bindings::get_op_Align();
            map.insert(bindings::get_op_code(op), Self::create_align);
            let op = bindings::get_op_Alloc();
            map.insert(bindings::get_op_code(op), Self::create_alloc);
            let op = bindings::get_op_Anchor();
            map.insert(bindings::get_op_code(op), Self::create_anchor);
            let op = bindings::get_op_And();
            map.insert(bindings::get_op_code(op), Self::create_and);
            let op = bindings::get_op_Bad();
            map.insert(bindings::get_op_code(op), Self::create_bad);
            let op = bindings::get_op_Bitcast();
            map.insert(bindings::get_op_code(op), Self::create_bitcast);
            let op = bindings::get_op_Block();
            map.insert(bindings::get_op_code(op), Self::create_block);
            let op = bindings::get_op_Builtin();
            map.insert(bindings::get_op_code(op), Self::create_builtin);
            let op = bindings::get_op_Call();
            map.insert(bindings::get_op_code(op), Self::create_call);
            let op = bindings::get_op_Cmp();
            map.insert(bindings::get_op_code(op), Self::create_cmp);
            let op = bindings::get_op_Cond();
            map.insert(bindings::get_op_code(op), Self::create_cond);
            let op = bindings::get_op_Confirm();
            map.insert(bindings::get_op_code(op), Self::create_confirm);
            let op = bindings::get_op_Const();
            map.insert(bindings::get_op_code(op), Self::create_const);
            let op = bindings::get_op_Conv();
            map.insert(bindings::get_op_code(op), Self::create_conv);
            let op = bindings::get_op_CopyB();
            map.insert(bindings::get_op_code(op), Self::create_copyb);
            let op = bindings::get_op_Deleted();
            map.insert(bindings::get_op_code(op), Self::create_deleted);
            let op = bindings::get_op_Div();
            map.insert(bindings::get_op_code(op), Self::create_div);
            let op = bindings::get_op_Dummy();
            map.insert(bindings::get_op_code(op), Self::create_dummy);
            let op = bindings::get_op_End();
            map.insert(bindings::get_op_code(op), Self::create_end);
            let op = bindings::get_op_Eor();
            map.insert(bindings::get_op_code(op), Self::create_eor);
            let op = bindings::get_op_Free();
            map.insert(bindings::get_op_code(op), Self::create_free);
            let op = bindings::get_op_IJmp();
            map.insert(bindings::get_op_code(op), Self::create_ijmp);
            let op = bindings::get_op_Id();
            map.insert(bindings::get_op_code(op), Self::create_id);
            let op = bindings::get_op_Jmp();
            map.insert(bindings::get_op_code(op), Self::create_jmp);
            let op = bindings::get_op_Load();
            map.insert(bindings::get_op_code(op), Self::create_load);
            let op = bindings::get_op_Member();
            map.insert(bindings::get_op_code(op), Self::create_member);
            let op = bindings::get_op_Minus();
            map.insert(bindings::get_op_code(op), Self::create_minus);
            let op = bindings::get_op_Mod();
            map.insert(bindings::get_op_code(op), Self::create_mod);
            let op = bindings::get_op_Mul();
            map.insert(bindings::get_op_code(op), Self::create_mul);
            let op = bindings::get_op_Mulh();
            map.insert(bindings::get_op_code(op), Self::create_mulh);
            let op = bindings::get_op_Mux();
            map.insert(bindings::get_op_code(op), Self::create_mux);
            let op = bindings::get_op_NoMem();
            map.insert(bindings::get_op_code(op), Self::create_nomem);
            let op = bindings::get_op_Not();
            map.insert(bindings::get_op_code(op), Self::create_not);
            let op = bindings::get_op_Offset();
            map.insert(bindings::get_op_code(op), Self::create_offset);
            let op = bindings::get_op_Or();
            map.insert(bindings::get_op_code(op), Self::create_or);
            let op = bindings::get_op_Phi();
            map.insert(bindings::get_op_code(op), Self::create_phi);
            let op = bindings::get_op_Pin();
            map.insert(bindings::get_op_code(op), Self::create_pin);
            let op = bindings::get_op_Proj();
            map.insert(bindings::get_op_code(op), Self::create_proj);
            let op = bindings::get_op_Raise();
            map.insert(bindings::get_op_code(op), Self::create_raise);
            let op = bindings::get_op_Return();
            map.insert(bindings::get_op_code(op), Self::create_return);
            let op = bindings::get_op_Sel();
            map.insert(bindings::get_op_code(op), Self::create_sel);
            let op = bindings::get_op_Shl();
            map.insert(bindings::get_op_code(op), Self::create_shl);
            let op = bindings::get_op_Shr();
            map.insert(bindings::get_op_code(op), Self::create_shr);
            let op = bindings::get_op_Shrs();
            map.insert(bindings::get_op_code(op), Self::create_shrs);
            let op = bindings::get_op_Size();
            map.insert(bindings::get_op_code(op), Self::create_size);
            let op = bindings::get_op_Start();
            map.insert(bindings::get_op_code(op), Self::create_start);
            let op = bindings::get_op_Store();
            map.insert(bindings::get_op_code(op), Self::create_store);
            let op = bindings::get_op_Sub();
            map.insert(bindings::get_op_code(op), Self::create_sub);
            let op = bindings::get_op_Switch();
            map.insert(bindings::get_op_code(op), Self::create_switch);
            let op = bindings::get_op_Sync();
            map.insert(bindings::get_op_code(op), Self::create_sync);
            let op = bindings::get_op_Tuple();
            map.insert(bindings::get_op_code(op), Self::create_tuple);
            let op = bindings::get_op_Unknown();
            map.insert(bindings::get_op_code(op), Self::create_unknown);
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

    pub fn proj_kind(proj: Proj) -> ProjKind {
        let pred = proj.pred();
        match pred {
            Node::Alloc(node) => match proj.num() {
                0 => ProjKind::Alloc_M(node),
                1 => ProjKind::Alloc_Res(node),
                _ => ProjKind::Other,
            },
            Node::Builtin(node) => match proj.num() {
                0 => ProjKind::Builtin_M(node),
                _ => ProjKind::Other,
            },
            Node::Call(node) => match proj.num() {
                0 => ProjKind::Call_M(node),
                1 => ProjKind::Call_TResult(node),
                2 => ProjKind::Call_XRegular(node),
                3 => ProjKind::Call_XExcept(node),
                _ => ProjKind::Other,
            },
            Node::Cond(node) => match proj.num() {
                0 => ProjKind::Cond_False(node),
                1 => ProjKind::Cond_True(node),
                _ => ProjKind::Other,
            },
            Node::Div(node) => match proj.num() {
                0 => ProjKind::Div_M(node),
                1 => ProjKind::Div_Res(node),
                2 => ProjKind::Div_XRegular(node),
                3 => ProjKind::Div_XExcept(node),
                _ => ProjKind::Other,
            },
            Node::Load(node) => match proj.num() {
                0 => ProjKind::Load_M(node),
                1 => ProjKind::Load_Res(node),
                2 => ProjKind::Load_XRegular(node),
                3 => ProjKind::Load_XExcept(node),
                _ => ProjKind::Other,
            },
            Node::Mod(node) => match proj.num() {
                0 => ProjKind::Mod_M(node),
                1 => ProjKind::Mod_Res(node),
                2 => ProjKind::Mod_XRegular(node),
                3 => ProjKind::Mod_XExcept(node),
                _ => ProjKind::Other,
            },
            Node::Raise(node) => match proj.num() {
                0 => ProjKind::Raise_M(node),
                1 => ProjKind::Raise_X(node),
                _ => ProjKind::Other,
            },
            Node::Start(node) => match proj.num() {
                0 => ProjKind::Start_M(node),
                1 => ProjKind::Start_PFrameBase(node),
                2 => ProjKind::Start_TArgs(node),
                _ => ProjKind::Other,
            },
            Node::Store(node) => match proj.num() {
                0 => ProjKind::Store_M(node),
                1 => ProjKind::Store_XRegular(node),
                2 => ProjKind::Store_XExcept(node),
                _ => ProjKind::Other,
            },
            Node::Switch(node) => match proj.num() {
                0 => ProjKind::Switch_Default(node),
                _ => ProjKind::Other,
            },
            Node::Proj(proj, ProjKind::Start_TArgs(start)) => {
                ProjKind::Start_TArgs_Arg(proj.num(), start, proj)
            }
            _ => ProjKind::Other,
        }
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
        let proj = Proj(ir_node);
        Node::Proj(proj, Self::proj_kind(proj))
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
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Add(*mut bindings::ir_node);

impl Add {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Add(ir_node) } == 0 {
            panic!("given ir_node is not a Add");
        }
        Add(ir_node)
    }

    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Add_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Add_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Add_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Add_right(self.0, unwrapped);
        }
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

impl fmt::Debug for Add {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Add {}", self.node_id())
    }
}
/// Symbolic constant that represents the address of an entity (variable or
/// method)
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Address(*mut bindings::ir_node);

impl Address {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Address(ir_node) } == 0 {
            panic!("given ir_node is not a Address");
        }
        Address(ir_node)
    }
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
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Align(*mut bindings::ir_node);

impl Align {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Align(ir_node) } == 0 {
            panic!("given ir_node is not a Align");
        }
        Align(ir_node)
    }
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

impl fmt::Debug for Align {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Align {}", self.node_id())
    }
}
/// Allocates a block of memory on the stack.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Alloc(*mut bindings::ir_node);

impl Alloc {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Alloc(ir_node) } == 0 {
            panic!("given ir_node is not a Alloc");
        }
        Alloc(ir_node)
    }

    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Alloc_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Alloc_mem(self.0, unwrapped);
        }
    }

    /// Gets size of the block in bytes.
    pub fn size(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Alloc_size(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets size of the block in bytes.
    pub fn set_size(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Alloc_size(self.0, unwrapped);
        }
    }

    /// Gets alignment of the memory block (must be a power of 2).
    pub fn alignment(&self) -> ::std::os::raw::c_uint {
        let unwrapped = unsafe { bindings::get_Alloc_alignment(self.0) };
        unwrapped
    }

    /// Sets alignment of the memory block (must be a power of 2).
    pub fn set_alignment(&self, val: ::std::os::raw::c_uint) {
        let unwrapped = val;
        unsafe {
            bindings::set_Alloc_alignment(self.0, unwrapped);
        }
    }

    /// memory result.
    pub fn new_proj_m(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::M, 0) })
    }

    /// pointer to newly allocated memory.
    pub fn new_proj_res(&self, mode: bindings::mode::Type) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, mode, 1) })
    }

    /// memory result.
    pub fn out_proj_m(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Alloc_M(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// pointer to newly allocated memory.
    pub fn out_proj_res(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Alloc_Res(_)) = out_node {
                return Some(proj);
            }
        }
        None
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

impl fmt::Debug for Alloc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Alloc {}", self.node_id())
    }
}
/// Utility node used to "hold" nodes in a graph that might possibly not be
/// reachable by other means or which should be reachable immediately without
/// searching through the graph.
/// Each firm-graph contains exactly one anchor node whose address is always
/// known. All other well-known graph-nodes like Start, End, NoMem, ...
/// are found by looking at the respective Anchor operand.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Anchor(*mut bindings::ir_node);

impl Anchor {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Anchor(ir_node) } == 0 {
            panic!("given ir_node is not a Anchor");
        }
        Anchor(ir_node)
    }

    /// Gets block the end node belongs to.
    pub fn end_block(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_end_block(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets block the end node belongs to.
    pub fn set_end_block(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Anchor_end_block(self.0, unwrapped);
        }
    }

    /// Gets block the start node belongs to.
    pub fn start_block(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_start_block(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets block the start node belongs to.
    pub fn set_start_block(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Anchor_start_block(self.0, unwrapped);
        }
    }

    /// Gets end node of this ir_graph.
    pub fn end(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_end(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets end node of this ir_graph.
    pub fn set_end(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Anchor_end(self.0, unwrapped);
        }
    }

    /// Gets start node of this ir_graph.
    pub fn start(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_start(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets start node of this ir_graph.
    pub fn set_start(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Anchor_start(self.0, unwrapped);
        }
    }

    /// Gets frame of this ir_graph.
    pub fn frame(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_frame(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets frame of this ir_graph.
    pub fn set_frame(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Anchor_frame(self.0, unwrapped);
        }
    }

    /// Gets initial memory of this ir_graph.
    pub fn initial_mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_initial_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets initial memory of this ir_graph.
    pub fn set_initial_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Anchor_initial_mem(self.0, unwrapped);
        }
    }

    /// Gets argument proj of the start node.
    pub fn args(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_args(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets argument proj of the start node.
    pub fn set_args(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Anchor_args(self.0, unwrapped);
        }
    }

    /// Gets the only NoMem node of this ir_graph.
    pub fn no_mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_no_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets the only NoMem node of this ir_graph.
    pub fn set_no_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Anchor_no_mem(self.0, unwrapped);
        }
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

impl fmt::Debug for Anchor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Anchor {}", self.node_id())
    }
}
/// returns the result of a bitwise and operation of its operands
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct And(*mut bindings::ir_node);

impl And {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_And(ir_node) } == 0 {
            panic!("given ir_node is not a And");
        }
        And(ir_node)
    }

    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_And_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_And_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_And_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_And_right(self.0, unwrapped);
        }
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

impl fmt::Debug for And {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "And {}", self.node_id())
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
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Bad(*mut bindings::ir_node);

impl Bad {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Bad(ir_node) } == 0 {
            panic!("given ir_node is not a Bad");
        }
        Bad(ir_node)
    }
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

impl fmt::Debug for Bad {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Bad {}", self.node_id())
    }
}
/// Converts a value between modes with different arithmetics but same
/// number of bits by reinterpreting the bits in the new mode
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Bitcast(*mut bindings::ir_node);

impl Bitcast {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Bitcast(ir_node) } == 0 {
            panic!("given ir_node is not a Bitcast");
        }
        Bitcast(ir_node)
    }

    /// Gets operand.
    pub fn op(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Bitcast_op(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets operand.
    pub fn set_op(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Bitcast_op(self.0, unwrapped);
        }
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

impl fmt::Debug for Bitcast {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Bitcast {}", self.node_id())
    }
}
/// A basic block
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Block(*mut bindings::ir_node);

impl Block {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Block(ir_node) } == 0 {
            panic!("given ir_node is not a Block");
        }
        Block(ir_node)
    }

    /// Gets entity representing this block.
    pub fn entity(&self) -> *mut bindings::ir_entity {
        let unwrapped = unsafe { bindings::get_Block_entity(self.0) };
        unwrapped
    }

    /// Sets entity representing this block.
    pub fn set_entity(&self, val: *mut bindings::ir_entity) {
        let unwrapped = val;
        unsafe {
            bindings::set_Block_entity(self.0, unwrapped);
        }
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

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Block {}", self.node_id())
    }
}
/// performs a backend-specific builtin.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Builtin(*mut bindings::ir_node);

impl Builtin {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Builtin(ir_node) } == 0 {
            panic!("given ir_node is not a Builtin");
        }
        Builtin(ir_node)
    }

    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Builtin_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Builtin_mem(self.0, unwrapped);
        }
    }

    /// Gets kind of builtin.
    pub fn kind(&self) -> bindings::ir_builtin_kind::Type {
        let unwrapped = unsafe { bindings::get_Builtin_kind(self.0) };
        unwrapped
    }

    /// Sets kind of builtin.
    pub fn set_kind(&self, val: bindings::ir_builtin_kind::Type) {
        let unwrapped = val;
        unsafe {
            bindings::set_Builtin_kind(self.0, unwrapped);
        }
    }

    /// Gets method type for the builtin call.
    pub fn ty(&self) -> *mut bindings::ir_type {
        let unwrapped = unsafe { bindings::get_Builtin_type(self.0) };
        unwrapped
    }

    /// Sets method type for the builtin call.
    pub fn set_ty(&self, val: *mut bindings::ir_type) {
        let unwrapped = val;
        unsafe {
            bindings::set_Builtin_type(self.0, unwrapped);
        }
    }

    /// memory result.
    pub fn new_proj_m(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::M, 0) })
    }

    /// memory result.
    pub fn out_proj_m(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Builtin_M(_)) = out_node {
                return Some(proj);
            }
        }
        None
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

impl fmt::Debug for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Builtin {}", self.node_id())
    }
}
/// Calls other code. Control flow is transferred to ptr, additional
/// operands are passed to the called code. Called code usually performs a
/// return operation. The operands of this return operation are the result
/// of the Call node.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Call(*mut bindings::ir_node);

impl Call {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Call(ir_node) } == 0 {
            panic!("given ir_node is not a Call");
        }
        Call(ir_node)
    }

    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Call_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Call_mem(self.0, unwrapped);
        }
    }

    /// Gets pointer to called code.
    pub fn ptr(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Call_ptr(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets pointer to called code.
    pub fn set_ptr(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Call_ptr(self.0, unwrapped);
        }
    }

    /// Gets type of the call (usually type of the called procedure).
    pub fn ty(&self) -> *mut bindings::ir_type {
        let unwrapped = unsafe { bindings::get_Call_type(self.0) };
        unwrapped
    }

    /// Sets type of the call (usually type of the called procedure).
    pub fn set_ty(&self, val: *mut bindings::ir_type) {
        let unwrapped = val;
        unsafe {
            bindings::set_Call_type(self.0, unwrapped);
        }
    }

    /// memory result.
    pub fn new_proj_m(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::M, 0) })
    }

    /// tuple containing all results.
    pub fn new_proj_t_result(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::T, 1) })
    }

    /// control flow when no exception occurs.
    pub fn new_proj_x_regular(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 2) })
    }

    /// control flow when exception occurred.
    pub fn new_proj_x_except(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 3) })
    }

    /// memory result.
    pub fn out_proj_m(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Call_M(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// tuple containing all results.
    pub fn out_proj_t_result(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Call_TResult(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow when no exception occurs.
    pub fn out_proj_x_regular(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Call_XRegular(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow when exception occurred.
    pub fn out_proj_x_except(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Call_XExcept(_)) = out_node {
                return Some(proj);
            }
        }
        None
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
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Cmp(*mut bindings::ir_node);

impl Cmp {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Cmp(ir_node) } == 0 {
            panic!("given ir_node is not a Cmp");
        }
        Cmp(ir_node)
    }

    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Cmp_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Cmp_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Cmp_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Cmp_right(self.0, unwrapped);
        }
    }

    /// Gets Comparison relation.
    pub fn relation(&self) -> bindings::ir_relation::Type {
        let unwrapped = unsafe { bindings::get_Cmp_relation(self.0) };
        unwrapped
    }

    /// Sets Comparison relation.
    pub fn set_relation(&self, val: bindings::ir_relation::Type) {
        let unwrapped = val;
        unsafe {
            bindings::set_Cmp_relation(self.0, unwrapped);
        }
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

impl fmt::Debug for Cmp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Cmp {}", self.node_id())
    }
}
/// Conditionally change control flow.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Cond(*mut bindings::ir_node);

impl Cond {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Cond(ir_node) } == 0 {
            panic!("given ir_node is not a Cond");
        }
        Cond(ir_node)
    }

    /// Gets condition parameter.
    pub fn selector(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Cond_selector(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets condition parameter.
    pub fn set_selector(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Cond_selector(self.0, unwrapped);
        }
    }

    /// Gets can indicate the most likely jump.
    pub fn jmp_pred(&self) -> bindings::cond_jmp_predicate::Type {
        let unwrapped = unsafe { bindings::get_Cond_jmp_pred(self.0) };
        unwrapped
    }

    /// Sets can indicate the most likely jump.
    pub fn set_jmp_pred(&self, val: bindings::cond_jmp_predicate::Type) {
        let unwrapped = val;
        unsafe {
            bindings::set_Cond_jmp_pred(self.0, unwrapped);
        }
    }

    /// control flow if operand is "false".
    pub fn new_proj_false(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 0) })
    }

    /// control flow if operand is "true".
    pub fn new_proj_true(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 1) })
    }

    /// control flow if operand is "false".
    pub fn out_proj_false(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Cond_False(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow if operand is "true".
    pub fn out_proj_true(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Cond_True(_)) = out_node {
                return Some(proj);
            }
        }
        None
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

impl fmt::Debug for Cond {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Cond {}", self.node_id())
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
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Confirm(*mut bindings::ir_node);

impl Confirm {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Confirm(ir_node) } == 0 {
            panic!("given ir_node is not a Confirm");
        }
        Confirm(ir_node)
    }

    /// Gets value to express a constraint for.
    pub fn value(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Confirm_value(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets value to express a constraint for.
    pub fn set_value(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Confirm_value(self.0, unwrapped);
        }
    }

    /// Gets value to compare against.
    pub fn bound(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Confirm_bound(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets value to compare against.
    pub fn set_bound(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Confirm_bound(self.0, unwrapped);
        }
    }

    /// Gets relation of value to bound.
    pub fn relation(&self) -> bindings::ir_relation::Type {
        let unwrapped = unsafe { bindings::get_Confirm_relation(self.0) };
        unwrapped
    }

    /// Sets relation of value to bound.
    pub fn set_relation(&self, val: bindings::ir_relation::Type) {
        let unwrapped = val;
        unsafe {
            bindings::set_Confirm_relation(self.0, unwrapped);
        }
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

impl fmt::Debug for Confirm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Confirm {}", self.node_id())
    }
}
/// Returns a constant value.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Const(*mut bindings::ir_node);

impl Const {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Const(ir_node) } == 0 {
            panic!("given ir_node is not a Const");
        }
        Const(ir_node)
    }

    /// Gets constant value (a tarval object).
    pub fn tarval(&self) -> *mut bindings::ir_tarval {
        let unwrapped = unsafe { bindings::get_Const_tarval(self.0) };
        unwrapped
    }

    /// Sets constant value (a tarval object).
    pub fn set_tarval(&self, val: *mut bindings::ir_tarval) {
        let unwrapped = val;
        unsafe {
            bindings::set_Const_tarval(self.0, unwrapped);
        }
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

impl fmt::Debug for Const {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Const {}", self.node_id())
    }
}
/// Converts values between modes
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Conv(*mut bindings::ir_node);

impl Conv {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Conv(ir_node) } == 0 {
            panic!("given ir_node is not a Conv");
        }
        Conv(ir_node)
    }

    /// Gets operand.
    pub fn op(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Conv_op(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets operand.
    pub fn set_op(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Conv_op(self.0, unwrapped);
        }
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

impl fmt::Debug for Conv {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Conv {}", self.node_id())
    }
}
/// Copies a block of memory with statically known size/type.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct CopyB(*mut bindings::ir_node);

impl CopyB {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_CopyB(ir_node) } == 0 {
            panic!("given ir_node is not a CopyB");
        }
        CopyB(ir_node)
    }

    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_CopyB_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_CopyB_mem(self.0, unwrapped);
        }
    }

    /// Gets destination address.
    pub fn dst(&self) -> Node {
        let unwrapped = unsafe { bindings::get_CopyB_dst(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets destination address.
    pub fn set_dst(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_CopyB_dst(self.0, unwrapped);
        }
    }

    /// Gets source address.
    pub fn src(&self) -> Node {
        let unwrapped = unsafe { bindings::get_CopyB_src(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets source address.
    pub fn set_src(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_CopyB_src(self.0, unwrapped);
        }
    }

    /// Gets type of copied data.
    pub fn ty(&self) -> *mut bindings::ir_type {
        let unwrapped = unsafe { bindings::get_CopyB_type(self.0) };
        unwrapped
    }

    /// Sets type of copied data.
    pub fn set_ty(&self, val: *mut bindings::ir_type) {
        let unwrapped = val;
        unsafe {
            bindings::set_CopyB_type(self.0, unwrapped);
        }
    }

    /// Gets volatile CopyB nodes have a visible side-effect and may not be
    /// optimized.
    pub fn volatility(&self) -> bindings::ir_volatility::Type {
        let unwrapped = unsafe { bindings::get_CopyB_volatility(self.0) };
        unwrapped
    }

    /// Sets volatile CopyB nodes have a visible side-effect and may not be
    /// optimized.
    pub fn set_volatility(&self, val: bindings::ir_volatility::Type) {
        let unwrapped = val;
        unsafe {
            bindings::set_CopyB_volatility(self.0, unwrapped);
        }
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

impl fmt::Debug for CopyB {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "CopyB {}", self.node_id())
    }
}
/// Internal node which is temporary set to nodes which are already removed
/// from the graph.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Deleted(*mut bindings::ir_node);

impl Deleted {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Deleted(ir_node) } == 0 {
            panic!("given ir_node is not a Deleted");
        }
        Deleted(ir_node)
    }
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

impl fmt::Debug for Deleted {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Deleted {}", self.node_id())
    }
}
/// returns the quotient of its 2 operands
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Div(*mut bindings::ir_node);

impl Div {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Div(ir_node) } == 0 {
            panic!("given ir_node is not a Div");
        }
        Div(ir_node)
    }

    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Div_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Div_mem(self.0, unwrapped);
        }
    }

    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Div_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Div_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Div_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Div_right(self.0, unwrapped);
        }
    }

    /// Gets mode of the result value.
    pub fn resmode(&self) -> *mut bindings::ir_mode {
        let unwrapped = unsafe { bindings::get_Div_resmode(self.0) };
        unwrapped
    }

    /// Sets mode of the result value.
    pub fn set_resmode(&self, val: *mut bindings::ir_mode) {
        let unwrapped = val;
        unsafe {
            bindings::set_Div_resmode(self.0, unwrapped);
        }
    }

    /// Gets Set when division remainder is known to be zero.
    pub fn no_remainder(&self) -> i32 {
        let unwrapped = unsafe { bindings::get_Div_no_remainder(self.0) };
        unwrapped
    }

    /// Sets Set when division remainder is known to be zero.
    pub fn set_no_remainder(&self, val: i32) {
        let unwrapped = val;
        unsafe {
            bindings::set_Div_no_remainder(self.0, unwrapped);
        }
    }

    /// memory result.
    pub fn new_proj_m(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::M, 0) })
    }

    /// result of computation.
    pub fn new_proj_res(&self, mode: bindings::mode::Type) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, mode, 1) })
    }

    /// control flow when no exception occurs.
    pub fn new_proj_x_regular(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 2) })
    }

    /// control flow when exception occurred.
    pub fn new_proj_x_except(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 3) })
    }

    /// memory result.
    pub fn out_proj_m(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Div_M(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// result of computation.
    pub fn out_proj_res(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Div_Res(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow when no exception occurs.
    pub fn out_proj_x_regular(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Div_XRegular(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow when exception occurred.
    pub fn out_proj_x_except(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Div_XExcept(_)) = out_node {
                return Some(proj);
            }
        }
        None
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

impl fmt::Debug for Div {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Div {}", self.node_id())
    }
}
/// A placeholder value. This is used when constructing cyclic graphs where
/// you have cases where not all predecessors of a phi-node are known. Dummy
/// nodes are used for the unknown predecessors and replaced later.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Dummy(*mut bindings::ir_node);

impl Dummy {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Dummy(ir_node) } == 0 {
            panic!("given ir_node is not a Dummy");
        }
        Dummy(ir_node)
    }
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

impl fmt::Debug for Dummy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Dummy {}", self.node_id())
    }
}
/// Last node of a graph. It references nodes in endless loops (so called
/// keepalive edges)
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct End(*mut bindings::ir_node);

impl End {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_End(ir_node) } == 0 {
            panic!("given ir_node is not a End");
        }
        End(ir_node)
    }
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

impl fmt::Debug for End {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "End {}", self.node_id())
    }
}
/// returns the result of a bitwise exclusive or operation of its operands.
///
/// This is also known as the Xor operation.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Eor(*mut bindings::ir_node);

impl Eor {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Eor(ir_node) } == 0 {
            panic!("given ir_node is not a Eor");
        }
        Eor(ir_node)
    }

    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Eor_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Eor_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Eor_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Eor_right(self.0, unwrapped);
        }
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

impl fmt::Debug for Eor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Eor {}", self.node_id())
    }
}
/// Frees a block of memory previously allocated by an Alloc node
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Free(*mut bindings::ir_node);

impl Free {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Free(ir_node) } == 0 {
            panic!("given ir_node is not a Free");
        }
        Free(ir_node)
    }

    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Free_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Free_mem(self.0, unwrapped);
        }
    }

    /// Gets pointer to the object to free.
    pub fn ptr(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Free_ptr(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets pointer to the object to free.
    pub fn set_ptr(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Free_ptr(self.0, unwrapped);
        }
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

impl fmt::Debug for Free {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Free {}", self.node_id())
    }
}
/// Jumps to the code in its argument. The code has to be in the same
/// function and the destination must be one of the blocks reachable
/// by the tuple results
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct IJmp(*mut bindings::ir_node);

impl IJmp {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_IJmp(ir_node) } == 0 {
            panic!("given ir_node is not a IJmp");
        }
        IJmp(ir_node)
    }

    /// Gets target address of the jump.
    pub fn target(&self) -> Node {
        let unwrapped = unsafe { bindings::get_IJmp_target(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets target address of the jump.
    pub fn set_target(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_IJmp_target(self.0, unwrapped);
        }
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

impl fmt::Debug for IJmp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "IJmp {}", self.node_id())
    }
}
/// Returns its operand unchanged.
///
/// This is mainly used when exchanging nodes. Usually you shouldn't see Id
/// nodes since the getters/setters for node inputs skip them automatically.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Id(*mut bindings::ir_node);

impl Id {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Id(ir_node) } == 0 {
            panic!("given ir_node is not a Id");
        }
        Id(ir_node)
    }

    /// Gets the value which is returned unchanged.
    pub fn pred(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Id_pred(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets the value which is returned unchanged.
    pub fn set_pred(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Id_pred(self.0, unwrapped);
        }
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

impl fmt::Debug for Id {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Id {}", self.node_id())
    }
}
/// Jumps to the block connected through the out-value
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Jmp(*mut bindings::ir_node);

impl Jmp {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Jmp(ir_node) } == 0 {
            panic!("given ir_node is not a Jmp");
        }
        Jmp(ir_node)
    }
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

impl fmt::Debug for Jmp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Jmp {}", self.node_id())
    }
}
/// Loads a value from memory (heap or stack).
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Load(*mut bindings::ir_node);

impl Load {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Load(ir_node) } == 0 {
            panic!("given ir_node is not a Load");
        }
        Load(ir_node)
    }

    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Load_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Load_mem(self.0, unwrapped);
        }
    }

    /// Gets address to load from.
    pub fn ptr(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Load_ptr(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets address to load from.
    pub fn set_ptr(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Load_ptr(self.0, unwrapped);
        }
    }

    /// Gets mode of the value to be loaded.
    pub fn mode(&self) -> *mut bindings::ir_mode {
        let unwrapped = unsafe { bindings::get_Load_mode(self.0) };
        unwrapped
    }

    /// Sets mode of the value to be loaded.
    pub fn set_mode(&self, val: *mut bindings::ir_mode) {
        let unwrapped = val;
        unsafe {
            bindings::set_Load_mode(self.0, unwrapped);
        }
    }

    /// Gets The type of the object which is stored at ptr (need not match with
    /// mode).
    pub fn ty(&self) -> *mut bindings::ir_type {
        let unwrapped = unsafe { bindings::get_Load_type(self.0) };
        unwrapped
    }

    /// Sets The type of the object which is stored at ptr (need not match with
    /// mode).
    pub fn set_ty(&self, val: *mut bindings::ir_type) {
        let unwrapped = val;
        unsafe {
            bindings::set_Load_type(self.0, unwrapped);
        }
    }

    /// Gets volatile loads are a visible side-effect and may not be optimized.
    pub fn volatility(&self) -> bindings::ir_volatility::Type {
        let unwrapped = unsafe { bindings::get_Load_volatility(self.0) };
        unwrapped
    }

    /// Sets volatile loads are a visible side-effect and may not be optimized.
    pub fn set_volatility(&self, val: bindings::ir_volatility::Type) {
        let unwrapped = val;
        unsafe {
            bindings::set_Load_volatility(self.0, unwrapped);
        }
    }

    /// Gets pointers to unaligned loads don't need to respect the
    /// load-mode/type alignments.
    pub fn unaligned(&self) -> bindings::ir_align::Type {
        let unwrapped = unsafe { bindings::get_Load_unaligned(self.0) };
        unwrapped
    }

    /// Sets pointers to unaligned loads don't need to respect the
    /// load-mode/type alignments.
    pub fn set_unaligned(&self, val: bindings::ir_align::Type) {
        let unwrapped = val;
        unsafe {
            bindings::set_Load_unaligned(self.0, unwrapped);
        }
    }

    /// memory result.
    pub fn new_proj_m(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::M, 0) })
    }

    /// result of load operation.
    pub fn new_proj_res(&self, mode: bindings::mode::Type) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, mode, 1) })
    }

    /// control flow when no exception occurs.
    pub fn new_proj_x_regular(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 2) })
    }

    /// control flow when exception occurred.
    pub fn new_proj_x_except(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 3) })
    }

    /// memory result.
    pub fn out_proj_m(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Load_M(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// result of load operation.
    pub fn out_proj_res(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Load_Res(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow when no exception occurs.
    pub fn out_proj_x_regular(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Load_XRegular(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow when exception occurred.
    pub fn out_proj_x_except(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Load_XExcept(_)) = out_node {
                return Some(proj);
            }
        }
        None
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

impl fmt::Debug for Load {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Load {}", self.node_id())
    }
}
/// Computes the address of a compound type member given the base address
/// of an instance of the compound type.
///
/// A Member node must only produce a NULL pointer if the ptr input is NULL.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Member(*mut bindings::ir_node);

impl Member {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Member(ir_node) } == 0 {
            panic!("given ir_node is not a Member");
        }
        Member(ir_node)
    }

    /// Gets pointer to object to select from.
    pub fn ptr(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Member_ptr(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets pointer to object to select from.
    pub fn set_ptr(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Member_ptr(self.0, unwrapped);
        }
    }

    /// Gets entity which is selected.
    pub fn entity(&self) -> *mut bindings::ir_entity {
        let unwrapped = unsafe { bindings::get_Member_entity(self.0) };
        unwrapped
    }

    /// Sets entity which is selected.
    pub fn set_entity(&self, val: *mut bindings::ir_entity) {
        let unwrapped = val;
        unsafe {
            bindings::set_Member_entity(self.0, unwrapped);
        }
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

impl fmt::Debug for Member {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Member {}", self.node_id())
    }
}
/// returns the additive inverse of its operand
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Minus(*mut bindings::ir_node);

impl Minus {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Minus(ir_node) } == 0 {
            panic!("given ir_node is not a Minus");
        }
        Minus(ir_node)
    }

    /// Gets operand.
    pub fn op(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Minus_op(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets operand.
    pub fn set_op(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Minus_op(self.0, unwrapped);
        }
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

impl fmt::Debug for Minus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Minus {}", self.node_id())
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
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Mod(*mut bindings::ir_node);

impl Mod {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Mod(ir_node) } == 0 {
            panic!("given ir_node is not a Mod");
        }
        Mod(ir_node)
    }

    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Mod_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Mod_mem(self.0, unwrapped);
        }
    }

    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Mod_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Mod_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Mod_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Mod_right(self.0, unwrapped);
        }
    }

    /// Gets mode of the result.
    pub fn resmode(&self) -> *mut bindings::ir_mode {
        let unwrapped = unsafe { bindings::get_Mod_resmode(self.0) };
        unwrapped
    }

    /// Sets mode of the result.
    pub fn set_resmode(&self, val: *mut bindings::ir_mode) {
        let unwrapped = val;
        unsafe {
            bindings::set_Mod_resmode(self.0, unwrapped);
        }
    }

    /// memory result.
    pub fn new_proj_m(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::M, 0) })
    }

    /// result of computation.
    pub fn new_proj_res(&self, mode: bindings::mode::Type) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, mode, 1) })
    }

    /// control flow when no exception occurs.
    pub fn new_proj_x_regular(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 2) })
    }

    /// control flow when exception occurred.
    pub fn new_proj_x_except(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 3) })
    }

    /// memory result.
    pub fn out_proj_m(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Mod_M(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// result of computation.
    pub fn out_proj_res(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Mod_Res(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow when no exception occurs.
    pub fn out_proj_x_regular(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Mod_XRegular(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow when exception occurred.
    pub fn out_proj_x_except(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Mod_XExcept(_)) = out_node {
                return Some(proj);
            }
        }
        None
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

impl fmt::Debug for Mod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Mod {}", self.node_id())
    }
}
/// returns the product of its operands
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Mul(*mut bindings::ir_node);

impl Mul {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Mul(ir_node) } == 0 {
            panic!("given ir_node is not a Mul");
        }
        Mul(ir_node)
    }

    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Mul_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Mul_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Mul_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Mul_right(self.0, unwrapped);
        }
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

impl fmt::Debug for Mul {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Mul {}", self.node_id())
    }
}
/// returns the upper word of the product of its operands (the part which
/// would not fit into the result mode of a normal Mul anymore)
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Mulh(*mut bindings::ir_node);

impl Mulh {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Mulh(ir_node) } == 0 {
            panic!("given ir_node is not a Mulh");
        }
        Mulh(ir_node)
    }

    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Mulh_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Mulh_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Mulh_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Mulh_right(self.0, unwrapped);
        }
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

impl fmt::Debug for Mulh {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Mulh {}", self.node_id())
    }
}
/// returns the false or true operand depending on the value of the sel
/// operand
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Mux(*mut bindings::ir_node);

impl Mux {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Mux(ir_node) } == 0 {
            panic!("given ir_node is not a Mux");
        }
        Mux(ir_node)
    }

    /// Gets value making the output selection.
    pub fn sel(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Mux_sel(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets value making the output selection.
    pub fn set_sel(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Mux_sel(self.0, unwrapped);
        }
    }

    /// Gets selected if sel input is false.
    pub fn false_(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Mux_false(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets selected if sel input is false.
    pub fn set_false_(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Mux_false(self.0, unwrapped);
        }
    }

    /// Gets selected if sel input is true.
    pub fn true_(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Mux_true(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets selected if sel input is true.
    pub fn set_true_(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Mux_true(self.0, unwrapped);
        }
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

impl fmt::Debug for Mux {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Mux {}", self.node_id())
    }
}
/// Placeholder node for cases where you don't need any memory input
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct NoMem(*mut bindings::ir_node);

impl NoMem {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_NoMem(ir_node) } == 0 {
            panic!("given ir_node is not a NoMem");
        }
        NoMem(ir_node)
    }
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

impl fmt::Debug for NoMem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NoMem {}", self.node_id())
    }
}
/// returns the bitwise complement of a value. Works for boolean values, too.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Not(*mut bindings::ir_node);

impl Not {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Not(ir_node) } == 0 {
            panic!("given ir_node is not a Not");
        }
        Not(ir_node)
    }

    /// Gets operand.
    pub fn op(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Not_op(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets operand.
    pub fn set_op(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Not_op(self.0, unwrapped);
        }
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

impl fmt::Debug for Not {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Not {}", self.node_id())
    }
}
/// Symbolic constant that represents the offset of an entity in its owner type.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Offset(*mut bindings::ir_node);

impl Offset {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Offset(ir_node) } == 0 {
            panic!("given ir_node is not a Offset");
        }
        Offset(ir_node)
    }
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

impl fmt::Debug for Offset {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Offset {}", self.node_id())
    }
}
/// returns the result of a bitwise or operation of its operands
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Or(*mut bindings::ir_node);

impl Or {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Or(ir_node) } == 0 {
            panic!("given ir_node is not a Or");
        }
        Or(ir_node)
    }

    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Or_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Or_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Or_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Or_right(self.0, unwrapped);
        }
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

impl fmt::Debug for Or {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Or {}", self.node_id())
    }
}
/// Choose a value based on control flow. A phi node has 1 input for each
/// predecessor of its block. If a block is entered from its nth predecessor
/// all phi nodes produce their nth input as result.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Phi(*mut bindings::ir_node);

impl Phi {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Phi(ir_node) } == 0 {
            panic!("given ir_node is not a Phi");
        }
        Phi(ir_node)
    }

    /// Gets whether Phi represents the observable effect of a (possibly)
    /// nonterminating loop.
    pub fn loop_(&self) -> i32 {
        let unwrapped = unsafe { bindings::get_Phi_loop(self.0) };
        unwrapped
    }

    /// Sets whether Phi represents the observable effect of a (possibly)
    /// nonterminating loop.
    pub fn set_loop_(&self, val: i32) {
        let unwrapped = val;
        unsafe {
            bindings::set_Phi_loop(self.0, unwrapped);
        }
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

impl fmt::Debug for Phi {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Phi {}", self.node_id())
    }
}
/// Pin the value of the node node in the current block. No users of the Pin
/// node can float above the Block of the Pin. The node cannot float behind
/// this block. Often used to Pin the NoMem node.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Pin(*mut bindings::ir_node);

impl Pin {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Pin(ir_node) } == 0 {
            panic!("given ir_node is not a Pin");
        }
        Pin(ir_node)
    }

    /// Gets value which is pinned.
    pub fn op(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Pin_op(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets value which is pinned.
    pub fn set_op(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Pin_op(self.0, unwrapped);
        }
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

impl fmt::Debug for Pin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Pin {}", self.node_id())
    }
}
/// returns an entry of a tuple value
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Proj(*mut bindings::ir_node);

impl Proj {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Proj(ir_node) } == 0 {
            panic!("given ir_node is not a Proj");
        }
        Proj(ir_node)
    }

    /// Gets the tuple value from which a part is extracted.
    pub fn pred(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Proj_pred(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets the tuple value from which a part is extracted.
    pub fn set_pred(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Proj_pred(self.0, unwrapped);
        }
    }

    /// Gets number of tuple component to be extracted.
    pub fn num(&self) -> ::std::os::raw::c_uint {
        let unwrapped = unsafe { bindings::get_Proj_num(self.0) };
        unwrapped
    }

    /// Sets number of tuple component to be extracted.
    pub fn set_num(&self, val: ::std::os::raw::c_uint) {
        let unwrapped = val;
        unsafe {
            bindings::set_Proj_num(self.0, unwrapped);
        }
    }
}

impl Into<Node> for Proj {
    fn into(self) -> Node {
        Node::Proj(self, NodeFactory::proj_kind(self))
    }
}

impl NodeTrait for Proj {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl fmt::Debug for Proj {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Proj {}", self.node_id())
    }
}
/// Raises an exception. Unconditional change of control flow. Writes an
/// explicit Except variable to memory to pass it to the exception handler.
/// Must be lowered to a Call to a runtime check function.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Raise(*mut bindings::ir_node);

impl Raise {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Raise(ir_node) } == 0 {
            panic!("given ir_node is not a Raise");
        }
        Raise(ir_node)
    }

    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Raise_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Raise_mem(self.0, unwrapped);
        }
    }

    /// Gets pointer to exception object to be thrown.
    pub fn exo_ptr(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Raise_exo_ptr(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets pointer to exception object to be thrown.
    pub fn set_exo_ptr(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Raise_exo_ptr(self.0, unwrapped);
        }
    }

    /// memory result.
    pub fn new_proj_m(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::M, 0) })
    }

    /// control flow to exception handler.
    pub fn new_proj_x(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 1) })
    }

    /// memory result.
    pub fn out_proj_m(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Raise_M(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow to exception handler.
    pub fn out_proj_x(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Raise_X(_)) = out_node {
                return Some(proj);
            }
        }
        None
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

impl fmt::Debug for Raise {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Raise {}", self.node_id())
    }
}
/// Returns from the current function. Takes memory and return values as
/// operands.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Return(*mut bindings::ir_node);

impl Return {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Return(ir_node) } == 0 {
            panic!("given ir_node is not a Return");
        }
        Return(ir_node)
    }

    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Return_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Return_mem(self.0, unwrapped);
        }
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

impl fmt::Debug for Return {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Return {}", self.node_id())
    }
}
/// Computes the address of an array element from the array base pointer and
/// an index.
///
/// A Sel node must only produce a NULL pointer if the ptr input is NULL.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Sel(*mut bindings::ir_node);

impl Sel {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Sel(ir_node) } == 0 {
            panic!("given ir_node is not a Sel");
        }
        Sel(ir_node)
    }

    /// Gets pointer to array to select from.
    pub fn ptr(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Sel_ptr(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets pointer to array to select from.
    pub fn set_ptr(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Sel_ptr(self.0, unwrapped);
        }
    }

    /// Gets index to select.
    pub fn index(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Sel_index(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets index to select.
    pub fn set_index(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Sel_index(self.0, unwrapped);
        }
    }

    /// Gets array type.
    pub fn ty(&self) -> *mut bindings::ir_type {
        let unwrapped = unsafe { bindings::get_Sel_type(self.0) };
        unwrapped
    }

    /// Sets array type.
    pub fn set_ty(&self, val: *mut bindings::ir_type) {
        let unwrapped = val;
        unsafe {
            bindings::set_Sel_type(self.0, unwrapped);
        }
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

impl fmt::Debug for Sel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Sel {}", self.node_id())
    }
}
/// Returns its first operands bits shifted left by the amount of the 2nd
/// operand.
/// The right input (shift amount) must be an unsigned integer value.
/// If the result mode has modulo_shift!=0, then the effective shift amount is
/// the right input modulo this modulo_shift amount.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Shl(*mut bindings::ir_node);

impl Shl {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Shl(ir_node) } == 0 {
            panic!("given ir_node is not a Shl");
        }
        Shl(ir_node)
    }

    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Shl_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Shl_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Shl_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Shl_right(self.0, unwrapped);
        }
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

impl fmt::Debug for Shl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Shl {}", self.node_id())
    }
}
/// Returns its first operands bits shifted right by the amount of the 2nd
/// operand. No special handling for the sign bit is performed (zero extension).
/// The right input (shift amount) must be an unsigned integer value.
/// If the result mode has modulo_shift!=0, then the effective shift amount is
/// the right input modulo this modulo_shift amount.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Shr(*mut bindings::ir_node);

impl Shr {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Shr(ir_node) } == 0 {
            panic!("given ir_node is not a Shr");
        }
        Shr(ir_node)
    }

    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Shr_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Shr_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Shr_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Shr_right(self.0, unwrapped);
        }
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

impl fmt::Debug for Shr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Shr {}", self.node_id())
    }
}
/// Returns its first operands bits shifted right by the amount of the 2nd
/// operand. The leftmost bit (usually the sign bit) stays the same
/// (sign extension).
/// The right input (shift amount) must be an unsigned integer value.
/// If the result mode has modulo_shift!=0, then the effective shift amount is
/// the right input modulo this modulo_shift amount.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Shrs(*mut bindings::ir_node);

impl Shrs {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Shrs(ir_node) } == 0 {
            panic!("given ir_node is not a Shrs");
        }
        Shrs(ir_node)
    }

    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Shrs_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Shrs_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Shrs_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Shrs_right(self.0, unwrapped);
        }
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

impl fmt::Debug for Shrs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Shrs {}", self.node_id())
    }
}
/// A symbolic constant that represents the size of a type
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Size(*mut bindings::ir_node);

impl Size {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Size(ir_node) } == 0 {
            panic!("given ir_node is not a Size");
        }
        Size(ir_node)
    }
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

impl fmt::Debug for Size {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Size {}", self.node_id())
    }
}
/// The first node of a graph. Execution starts with this node.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Start(*mut bindings::ir_node);

impl Start {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Start(ir_node) } == 0 {
            panic!("given ir_node is not a Start");
        }
        Start(ir_node)
    }

    /// initial memory.
    pub fn new_proj_m(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::M, 0) })
    }

    /// frame base pointer.
    pub fn new_proj_p_frame_base(&self, mode: bindings::mode::Type) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, mode, 1) })
    }

    /// function arguments.
    pub fn new_proj_t_args(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::T, 2) })
    }

    /// initial memory.
    pub fn out_proj_m(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Start_M(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// frame base pointer.
    pub fn out_proj_p_frame_base(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Start_PFrameBase(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// function arguments.
    pub fn out_proj_t_args(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Start_TArgs(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }
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

impl fmt::Debug for Start {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Start {}", self.node_id())
    }
}
/// Stores a value into memory (heap or stack).
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Store(*mut bindings::ir_node);

impl Store {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Store(ir_node) } == 0 {
            panic!("given ir_node is not a Store");
        }
        Store(ir_node)
    }

    /// Gets memory dependency.
    pub fn mem(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Store_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    pub fn set_mem(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Store_mem(self.0, unwrapped);
        }
    }

    /// Gets address to store to.
    pub fn ptr(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Store_ptr(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets address to store to.
    pub fn set_ptr(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Store_ptr(self.0, unwrapped);
        }
    }

    /// Gets value to store.
    pub fn value(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Store_value(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets value to store.
    pub fn set_value(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Store_value(self.0, unwrapped);
        }
    }

    /// Gets The type of the object which is stored at ptr (need not match with
    /// value's type).
    pub fn ty(&self) -> *mut bindings::ir_type {
        let unwrapped = unsafe { bindings::get_Store_type(self.0) };
        unwrapped
    }

    /// Sets The type of the object which is stored at ptr (need not match with
    /// value's type).
    pub fn set_ty(&self, val: *mut bindings::ir_type) {
        let unwrapped = val;
        unsafe {
            bindings::set_Store_type(self.0, unwrapped);
        }
    }

    /// Gets volatile stores are a visible side-effect and may not be optimized.
    pub fn volatility(&self) -> bindings::ir_volatility::Type {
        let unwrapped = unsafe { bindings::get_Store_volatility(self.0) };
        unwrapped
    }

    /// Sets volatile stores are a visible side-effect and may not be optimized.
    pub fn set_volatility(&self, val: bindings::ir_volatility::Type) {
        let unwrapped = val;
        unsafe {
            bindings::set_Store_volatility(self.0, unwrapped);
        }
    }

    /// Gets pointers to unaligned stores don't need to respect the
    /// load-mode/type alignments.
    pub fn unaligned(&self) -> bindings::ir_align::Type {
        let unwrapped = unsafe { bindings::get_Store_unaligned(self.0) };
        unwrapped
    }

    /// Sets pointers to unaligned stores don't need to respect the
    /// load-mode/type alignments.
    pub fn set_unaligned(&self, val: bindings::ir_align::Type) {
        let unwrapped = val;
        unsafe {
            bindings::set_Store_unaligned(self.0, unwrapped);
        }
    }

    /// memory result.
    pub fn new_proj_m(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::M, 0) })
    }

    /// control flow when no exception occurs.
    pub fn new_proj_x_regular(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 1) })
    }

    /// control flow when exception occurred.
    pub fn new_proj_x_except(&self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 2) })
    }

    /// memory result.
    pub fn out_proj_m(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Store_M(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow when no exception occurs.
    pub fn out_proj_x_regular(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Store_XRegular(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow when exception occurred.
    pub fn out_proj_x_except(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Store_XExcept(_)) = out_node {
                return Some(proj);
            }
        }
        None
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

impl fmt::Debug for Store {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Store {}", self.node_id())
    }
}
/// returns the difference of its operands
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Sub(*mut bindings::ir_node);

impl Sub {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Sub(ir_node) } == 0 {
            panic!("given ir_node is not a Sub");
        }
        Sub(ir_node)
    }

    /// Gets first operand.
    pub fn left(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Sub_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    pub fn set_left(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Sub_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    pub fn right(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Sub_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    pub fn set_right(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Sub_right(self.0, unwrapped);
        }
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

impl fmt::Debug for Sub {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Sub {}", self.node_id())
    }
}
/// Change control flow. The destination is chosen based on an integer
/// input value which is looked up in a table.
///
/// Backends can implement this efficiently using a jump table.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Switch(*mut bindings::ir_node);

impl Switch {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Switch(ir_node) } == 0 {
            panic!("given ir_node is not a Switch");
        }
        Switch(ir_node)
    }

    /// Gets input selector.
    pub fn selector(&self) -> Node {
        let unwrapped = unsafe { bindings::get_Switch_selector(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets input selector.
    pub fn set_selector(&self, val: &'_ Node) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Switch_selector(self.0, unwrapped);
        }
    }

    /// Gets number of outputs (including pn_Switch_default).
    pub fn n_outs(&self) -> ::std::os::raw::c_uint {
        let unwrapped = unsafe { bindings::get_Switch_n_outs(self.0) };
        unwrapped
    }

    /// Sets number of outputs (including pn_Switch_default).
    pub fn set_n_outs(&self, val: ::std::os::raw::c_uint) {
        let unwrapped = val;
        unsafe {
            bindings::set_Switch_n_outs(self.0, unwrapped);
        }
    }

    /// Gets table describing mapping from input values to Proj numbers.
    pub fn table(&self) -> *mut bindings::ir_switch_table {
        let unwrapped = unsafe { bindings::get_Switch_table(self.0) };
        unwrapped
    }

    /// Sets table describing mapping from input values to Proj numbers.
    pub fn set_table(&self, val: *mut bindings::ir_switch_table) {
        let unwrapped = val;
        unsafe {
            bindings::set_Switch_table(self.0, unwrapped);
        }
    }

    /// control flow if no other case matches.
    pub fn new_proj_default(&self, mode: bindings::mode::Type) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, mode, 0) })
    }

    /// control flow if no other case matches.
    pub fn out_proj_default(&self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Switch_Default(_)) = out_node {
                return Some(proj);
            }
        }
        None
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

impl fmt::Debug for Switch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Switch {}", self.node_id())
    }
}
/// The Sync operation unifies several partial memory blocks. These blocks
/// have to be pairwise disjunct or the values in common locations have to
/// be identical.  This operation allows to specify all operations that
/// eventually need several partial memory blocks as input with a single
/// entrance by unifying the memories with a preceding Sync operation.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Sync(*mut bindings::ir_node);

impl Sync {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Sync(ir_node) } == 0 {
            panic!("given ir_node is not a Sync");
        }
        Sync(ir_node)
    }
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

impl fmt::Debug for Sync {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Sync {}", self.node_id())
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
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Tuple(*mut bindings::ir_node);

impl Tuple {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Tuple(ir_node) } == 0 {
            panic!("given ir_node is not a Tuple");
        }
        Tuple(ir_node)
    }
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

impl fmt::Debug for Tuple {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Tuple {}", self.node_id())
    }
}
/// Returns an unknown (at compile- and runtime) value. It is a valid
/// optimization to replace an Unknown by any other constant value.
///
/// Be careful when optimising Unknown values, you cannot simply replace
/// Unknown+x or Unknown<x with a new Unknown node if there are multiple
/// users of the original unknown node!
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Unknown(*mut bindings::ir_node);

impl Unknown {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Unknown(ir_node) } == 0 {
            panic!("given ir_node is not a Unknown");
        }
        Unknown(ir_node)
    }
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

impl fmt::Debug for Unknown {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Unknown {}", self.node_id())
    }
}
impl Graph {
    /// Creates a new Add-node.
    /// * `block` The block.
    /// * `irn_left` left
    /// * `irn_right` right
    pub fn new_add(&self, block: &'_ Block, irn_left: &'_ Node, irn_right: &'_ Node) -> Add {
        let ir_node = unsafe {
            bindings::new_r_Add(
                block.internal_ir_node(),
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
            )
        };
        Add::new(ir_node)
    }

    /// Creates a new Address-node.
    /// * `entity` entity to operate on
    pub fn new_address(&self, entity: *mut bindings::ir_entity) -> Address {
        let ir_node = unsafe { bindings::new_r_Address(self.irg, entity) };
        Address::new(ir_node)
    }

    /// Creates a new Align-node.
    /// * `mode` mode of the operations result
    /// * `ty` type to operate on
    pub fn new_align(&self, mode: *mut bindings::ir_mode, ty: *mut bindings::ir_type) -> Align {
        let ir_node = unsafe { bindings::new_r_Align(self.irg, mode, ty) };
        Align::new(ir_node)
    }

    /// Creates a new Alloc-node.
    /// * `block` The block.
    /// * `irn_mem` mem
    /// * `irn_size` size
    /// * `alignment` alignment of the memory block (must be a power of 2)
    pub fn new_alloc(
        &self,
        block: &'_ Block,
        irn_mem: &'_ Node,
        irn_size: &'_ Node,
        alignment: ::std::os::raw::c_uint,
    ) -> Alloc {
        let ir_node = unsafe {
            bindings::new_r_Alloc(
                block.internal_ir_node(),
                irn_mem.internal_ir_node(),
                irn_size.internal_ir_node(),
                alignment,
            )
        };
        Alloc::new(ir_node)
    }

    /// Creates a new And-node.
    /// * `block` The block.
    /// * `irn_left` left
    /// * `irn_right` right
    pub fn new_and(&self, block: &'_ Block, irn_left: &'_ Node, irn_right: &'_ Node) -> And {
        let ir_node = unsafe {
            bindings::new_r_And(
                block.internal_ir_node(),
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
            )
        };
        And::new(ir_node)
    }

    /// Creates a new Bad-node.
    /// * `mode` mode of the operations result
    pub fn new_bad(&self, mode: *mut bindings::ir_mode) -> Bad {
        let ir_node = unsafe { bindings::new_r_Bad(self.irg, mode) };
        Bad::new(ir_node)
    }

    /// Creates a new Bitcast-node.
    /// * `block` The block.
    /// * `irn_op` op
    /// * `mode` mode of the operations result
    pub fn new_bitcast(
        &self,
        block: &'_ Block,
        irn_op: &'_ Node,
        mode: *mut bindings::ir_mode,
    ) -> Bitcast {
        let ir_node = unsafe {
            bindings::new_r_Bitcast(block.internal_ir_node(), irn_op.internal_ir_node(), mode)
        };
        Bitcast::new(ir_node)
    }

    /// Creates a new Block-node.
    /// * `in_` additional inputs
    pub fn new_block(&self, in_: Vec<Node>) -> Block {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe { bindings::new_r_Block(self.irg, in_.len() as i32, in_.as_ptr()) };
        Block::new(ir_node)
    }

    /// Creates a new Builtin-node.
    /// * `block` The block.
    /// * `irn_mem` mem
    /// * `in_` additional inputs
    /// * `kind` kind of builtin
    /// * `ty` method type for the builtin call
    pub fn new_builtin(
        &self,
        block: &'_ Block,
        irn_mem: &'_ Node,
        in_: Vec<Node>,
        kind: bindings::ir_builtin_kind::Type,
        ty: *mut bindings::ir_type,
    ) -> Builtin {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe {
            bindings::new_r_Builtin(
                block.internal_ir_node(),
                irn_mem.internal_ir_node(),
                in_.len() as i32,
                in_.as_ptr(),
                kind,
                ty,
            )
        };
        Builtin::new(ir_node)
    }

    /// Creates a new Call-node.
    /// * `block` The block.
    /// * `irn_mem` mem
    /// * `irn_ptr` ptr
    /// * `in_` additional inputs
    /// * `ty` type of the call (usually type of the called procedure)
    pub fn new_call(
        &self,
        block: &'_ Block,
        irn_mem: &'_ Node,
        irn_ptr: &'_ Node,
        in_: Vec<Node>,
        ty: *mut bindings::ir_type,
    ) -> Call {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe {
            bindings::new_r_Call(
                block.internal_ir_node(),
                irn_mem.internal_ir_node(),
                irn_ptr.internal_ir_node(),
                in_.len() as i32,
                in_.as_ptr(),
                ty,
            )
        };
        Call::new(ir_node)
    }

    /// Creates a new Cmp-node.
    /// * `block` The block.
    /// * `irn_left` left
    /// * `irn_right` right
    /// * `relation` Comparison relation
    pub fn new_cmp(
        &self,
        block: &'_ Block,
        irn_left: &'_ Node,
        irn_right: &'_ Node,
        relation: bindings::ir_relation::Type,
    ) -> Cmp {
        let ir_node = unsafe {
            bindings::new_r_Cmp(
                block.internal_ir_node(),
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
                relation,
            )
        };
        Cmp::new(ir_node)
    }

    /// Creates a new Cond-node.
    /// * `block` The block.
    /// * `irn_selector` selector
    pub fn new_cond(&self, block: &'_ Block, irn_selector: &'_ Node) -> Cond {
        let ir_node = unsafe {
            bindings::new_r_Cond(block.internal_ir_node(), irn_selector.internal_ir_node())
        };
        Cond::new(ir_node)
    }

    /// Creates a new Confirm-node.
    /// * `block` The block.
    /// * `irn_value` value
    /// * `irn_bound` bound
    /// * `relation` relation of value to bound
    pub fn new_confirm(
        &self,
        block: &'_ Block,
        irn_value: &'_ Node,
        irn_bound: &'_ Node,
        relation: bindings::ir_relation::Type,
    ) -> Confirm {
        let ir_node = unsafe {
            bindings::new_r_Confirm(
                block.internal_ir_node(),
                irn_value.internal_ir_node(),
                irn_bound.internal_ir_node(),
                relation,
            )
        };
        Confirm::new(ir_node)
    }

    /// Creates a new Const-node.
    /// * `tarval` constant value (a tarval object)
    pub fn new_const(&self, tarval: *mut bindings::ir_tarval) -> Const {
        let ir_node = unsafe { bindings::new_r_Const(self.irg, tarval) };
        Const::new(ir_node)
    }

    /// Creates a new Conv-node.
    /// * `block` The block.
    /// * `irn_op` op
    /// * `mode` mode of the operations result
    pub fn new_conv(
        &self,
        block: &'_ Block,
        irn_op: &'_ Node,
        mode: *mut bindings::ir_mode,
    ) -> Conv {
        let ir_node = unsafe {
            bindings::new_r_Conv(block.internal_ir_node(), irn_op.internal_ir_node(), mode)
        };
        Conv::new(ir_node)
    }

    /// Creates a new CopyB-node.
    /// * `block` The block.
    /// * `irn_mem` mem
    /// * `irn_dst` dst
    /// * `irn_src` src
    /// * `ty` type of copied data
    /// * `flags` specifies volatility
    pub fn new_copyb(
        &self,
        block: &'_ Block,
        irn_mem: &'_ Node,
        irn_dst: &'_ Node,
        irn_src: &'_ Node,
        ty: *mut bindings::ir_type,
        flags: bindings::ir_cons_flags::Type,
    ) -> CopyB {
        let ir_node = unsafe {
            bindings::new_r_CopyB(
                block.internal_ir_node(),
                irn_mem.internal_ir_node(),
                irn_dst.internal_ir_node(),
                irn_src.internal_ir_node(),
                ty,
                flags,
            )
        };
        CopyB::new(ir_node)
    }

    /// Creates a new Div-node.
    /// * `block` The block.
    /// * `irn_mem` mem
    /// * `irn_left` left
    /// * `irn_right` right
    /// * `pinned` pinned state
    pub fn new_div(
        &self,
        block: &'_ Block,
        irn_mem: &'_ Node,
        irn_left: &'_ Node,
        irn_right: &'_ Node,
        pinned: i32,
    ) -> Div {
        let ir_node = unsafe {
            bindings::new_r_Div(
                block.internal_ir_node(),
                irn_mem.internal_ir_node(),
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
                pinned,
            )
        };
        Div::new(ir_node)
    }

    /// Creates a new Dummy-node.
    /// * `mode` mode of the operations result
    pub fn new_dummy(&self, mode: *mut bindings::ir_mode) -> Dummy {
        let ir_node = unsafe { bindings::new_r_Dummy(self.irg, mode) };
        Dummy::new(ir_node)
    }

    /// Creates a new End-node.
    /// * `in_` additional inputs
    pub fn new_end(&self, in_: Vec<Node>) -> End {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe { bindings::new_r_End(self.irg, in_.len() as i32, in_.as_ptr()) };
        End::new(ir_node)
    }

    /// Creates a new Eor-node.
    /// * `block` The block.
    /// * `irn_left` left
    /// * `irn_right` right
    pub fn new_eor(&self, block: &'_ Block, irn_left: &'_ Node, irn_right: &'_ Node) -> Eor {
        let ir_node = unsafe {
            bindings::new_r_Eor(
                block.internal_ir_node(),
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
            )
        };
        Eor::new(ir_node)
    }

    /// Creates a new Free-node.
    /// * `block` The block.
    /// * `irn_mem` mem
    /// * `irn_ptr` ptr
    pub fn new_free(&self, block: &'_ Block, irn_mem: &'_ Node, irn_ptr: &'_ Node) -> Free {
        let ir_node = unsafe {
            bindings::new_r_Free(
                block.internal_ir_node(),
                irn_mem.internal_ir_node(),
                irn_ptr.internal_ir_node(),
            )
        };
        Free::new(ir_node)
    }

    /// Creates a new IJmp-node.
    /// * `block` The block.
    /// * `irn_target` target
    pub fn new_ijmp(&self, block: &'_ Block, irn_target: &'_ Node) -> IJmp {
        let ir_node = unsafe {
            bindings::new_r_IJmp(block.internal_ir_node(), irn_target.internal_ir_node())
        };
        IJmp::new(ir_node)
    }

    /// Creates a new Jmp-node.
    /// * `block` The block.
    pub fn new_jmp(&self, block: &'_ Block) -> Jmp {
        let ir_node = unsafe { bindings::new_r_Jmp(block.internal_ir_node()) };
        Jmp::new(ir_node)
    }

    /// Creates a new Load-node.
    /// * `block` The block.
    /// * `irn_mem` mem
    /// * `irn_ptr` ptr
    /// * `mode` mode of the value to be loaded
    /// * `ty` The type of the object which is stored at ptr (need not match
    /// with mode) * `flags` specifies alignment, volatility and pin state
    pub fn new_load(
        &self,
        block: &'_ Block,
        irn_mem: &'_ Node,
        irn_ptr: &'_ Node,
        mode: *mut bindings::ir_mode,
        ty: *mut bindings::ir_type,
        flags: bindings::ir_cons_flags::Type,
    ) -> Load {
        let ir_node = unsafe {
            bindings::new_r_Load(
                block.internal_ir_node(),
                irn_mem.internal_ir_node(),
                irn_ptr.internal_ir_node(),
                mode,
                ty,
                flags,
            )
        };
        Load::new(ir_node)
    }

    /// Creates a new Member-node.
    /// * `block` The block.
    /// * `irn_ptr` ptr
    /// * `entity` entity which is selected
    pub fn new_member(
        &self,
        block: &'_ Block,
        irn_ptr: &'_ Node,
        entity: *mut bindings::ir_entity,
    ) -> Member {
        let ir_node = unsafe {
            bindings::new_r_Member(block.internal_ir_node(), irn_ptr.internal_ir_node(), entity)
        };
        Member::new(ir_node)
    }

    /// Creates a new Minus-node.
    /// * `block` The block.
    /// * `irn_op` op
    pub fn new_minus(&self, block: &'_ Block, irn_op: &'_ Node) -> Minus {
        let ir_node =
            unsafe { bindings::new_r_Minus(block.internal_ir_node(), irn_op.internal_ir_node()) };
        Minus::new(ir_node)
    }

    /// Creates a new Mod-node.
    /// * `block` The block.
    /// * `irn_mem` mem
    /// * `irn_left` left
    /// * `irn_right` right
    /// * `pinned` pinned state
    pub fn new_mod(
        &self,
        block: &'_ Block,
        irn_mem: &'_ Node,
        irn_left: &'_ Node,
        irn_right: &'_ Node,
        pinned: i32,
    ) -> Mod {
        let ir_node = unsafe {
            bindings::new_r_Mod(
                block.internal_ir_node(),
                irn_mem.internal_ir_node(),
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
                pinned,
            )
        };
        Mod::new(ir_node)
    }

    /// Creates a new Mul-node.
    /// * `block` The block.
    /// * `irn_left` left
    /// * `irn_right` right
    pub fn new_mul(&self, block: &'_ Block, irn_left: &'_ Node, irn_right: &'_ Node) -> Mul {
        let ir_node = unsafe {
            bindings::new_r_Mul(
                block.internal_ir_node(),
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
            )
        };
        Mul::new(ir_node)
    }

    /// Creates a new Mulh-node.
    /// * `block` The block.
    /// * `irn_left` left
    /// * `irn_right` right
    pub fn new_mulh(&self, block: &'_ Block, irn_left: &'_ Node, irn_right: &'_ Node) -> Mulh {
        let ir_node = unsafe {
            bindings::new_r_Mulh(
                block.internal_ir_node(),
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
            )
        };
        Mulh::new(ir_node)
    }

    /// Creates a new Mux-node.
    /// * `block` The block.
    /// * `irn_sel` sel
    /// * `irn_false` false
    /// * `irn_true` true
    pub fn new_mux(
        &self,
        block: &'_ Block,
        irn_sel: &'_ Node,
        irn_false: &'_ Node,
        irn_true: &'_ Node,
    ) -> Mux {
        let ir_node = unsafe {
            bindings::new_r_Mux(
                block.internal_ir_node(),
                irn_sel.internal_ir_node(),
                irn_false.internal_ir_node(),
                irn_true.internal_ir_node(),
            )
        };
        Mux::new(ir_node)
    }

    /// Creates a new NoMem-node.
    pub fn new_nomem(&self) -> NoMem {
        let ir_node = unsafe { bindings::new_r_NoMem(self.irg) };
        NoMem::new(ir_node)
    }

    /// Creates a new Not-node.
    /// * `block` The block.
    /// * `irn_op` op
    pub fn new_not(&self, block: &'_ Block, irn_op: &'_ Node) -> Not {
        let ir_node =
            unsafe { bindings::new_r_Not(block.internal_ir_node(), irn_op.internal_ir_node()) };
        Not::new(ir_node)
    }

    /// Creates a new Offset-node.
    /// * `mode` mode of the operations result
    /// * `entity` entity to operate on
    pub fn new_offset(
        &self,
        mode: *mut bindings::ir_mode,
        entity: *mut bindings::ir_entity,
    ) -> Offset {
        let ir_node = unsafe { bindings::new_r_Offset(self.irg, mode, entity) };
        Offset::new(ir_node)
    }

    /// Creates a new Or-node.
    /// * `block` The block.
    /// * `irn_left` left
    /// * `irn_right` right
    pub fn new_or(&self, block: &'_ Block, irn_left: &'_ Node, irn_right: &'_ Node) -> Or {
        let ir_node = unsafe {
            bindings::new_r_Or(
                block.internal_ir_node(),
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
            )
        };
        Or::new(ir_node)
    }

    /// Creates a new Phi-node.
    /// * `block` The block.
    /// * `in_` additional inputs
    /// * `mode` mode of the operations result
    pub fn new_phi(&self, block: &'_ Block, in_: Vec<Node>, mode: *mut bindings::ir_mode) -> Phi {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe {
            bindings::new_r_Phi(
                block.internal_ir_node(),
                in_.len() as i32,
                in_.as_ptr(),
                mode,
            )
        };
        Phi::new(ir_node)
    }

    /// Creates a new Pin-node.
    /// * `block` The block.
    /// * `irn_op` op
    pub fn new_pin(&self, block: &'_ Block, irn_op: &'_ Node) -> Pin {
        let ir_node =
            unsafe { bindings::new_r_Pin(block.internal_ir_node(), irn_op.internal_ir_node()) };
        Pin::new(ir_node)
    }

    /// Creates a new Proj-node.
    /// * `irn_pred` pred
    /// * `mode` mode of the operations result
    /// * `num` number of tuple component to be extracted
    pub fn new_proj(
        &self,
        irn_pred: &'_ Node,
        mode: *mut bindings::ir_mode,
        num: ::std::os::raw::c_uint,
    ) -> Proj {
        let ir_node = unsafe { bindings::new_r_Proj(irn_pred.internal_ir_node(), mode, num) };
        Proj::new(ir_node)
    }

    /// Creates a new Raise-node.
    /// * `block` The block.
    /// * `irn_mem` mem
    /// * `irn_exo_ptr` exo_ptr
    pub fn new_raise(&self, block: &'_ Block, irn_mem: &'_ Node, irn_exo_ptr: &'_ Node) -> Raise {
        let ir_node = unsafe {
            bindings::new_r_Raise(
                block.internal_ir_node(),
                irn_mem.internal_ir_node(),
                irn_exo_ptr.internal_ir_node(),
            )
        };
        Raise::new(ir_node)
    }

    /// Creates a new Return-node.
    /// * `block` The block.
    /// * `irn_mem` mem
    /// * `in_` additional inputs
    pub fn new_return(&self, block: &'_ Block, irn_mem: &'_ Node, in_: Vec<Node>) -> Return {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe {
            bindings::new_r_Return(
                block.internal_ir_node(),
                irn_mem.internal_ir_node(),
                in_.len() as i32,
                in_.as_ptr(),
            )
        };
        Return::new(ir_node)
    }

    /// Creates a new Sel-node.
    /// * `block` The block.
    /// * `irn_ptr` ptr
    /// * `irn_index` index
    /// * `ty` array type
    pub fn new_sel(
        &self,
        block: &'_ Block,
        irn_ptr: &'_ Node,
        irn_index: &'_ Node,
        ty: *mut bindings::ir_type,
    ) -> Sel {
        let ir_node = unsafe {
            bindings::new_r_Sel(
                block.internal_ir_node(),
                irn_ptr.internal_ir_node(),
                irn_index.internal_ir_node(),
                ty,
            )
        };
        Sel::new(ir_node)
    }

    /// Creates a new Shl-node.
    /// * `block` The block.
    /// * `irn_left` left
    /// * `irn_right` right
    pub fn new_shl(&self, block: &'_ Block, irn_left: &'_ Node, irn_right: &'_ Node) -> Shl {
        let ir_node = unsafe {
            bindings::new_r_Shl(
                block.internal_ir_node(),
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
            )
        };
        Shl::new(ir_node)
    }

    /// Creates a new Shr-node.
    /// * `block` The block.
    /// * `irn_left` left
    /// * `irn_right` right
    pub fn new_shr(&self, block: &'_ Block, irn_left: &'_ Node, irn_right: &'_ Node) -> Shr {
        let ir_node = unsafe {
            bindings::new_r_Shr(
                block.internal_ir_node(),
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
            )
        };
        Shr::new(ir_node)
    }

    /// Creates a new Shrs-node.
    /// * `block` The block.
    /// * `irn_left` left
    /// * `irn_right` right
    pub fn new_shrs(&self, block: &'_ Block, irn_left: &'_ Node, irn_right: &'_ Node) -> Shrs {
        let ir_node = unsafe {
            bindings::new_r_Shrs(
                block.internal_ir_node(),
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
            )
        };
        Shrs::new(ir_node)
    }

    /// Creates a new Size-node.
    /// * `mode` mode of the operations result
    /// * `ty` type to operate on
    pub fn new_size(&self, mode: *mut bindings::ir_mode, ty: *mut bindings::ir_type) -> Size {
        let ir_node = unsafe { bindings::new_r_Size(self.irg, mode, ty) };
        Size::new(ir_node)
    }

    /// Creates a new Start-node.
    pub fn new_start(&self) -> Start {
        let ir_node = unsafe { bindings::new_r_Start(self.irg) };
        Start::new(ir_node)
    }

    /// Creates a new Store-node.
    /// * `block` The block.
    /// * `irn_mem` mem
    /// * `irn_ptr` ptr
    /// * `irn_value` value
    /// * `ty` The type of the object which is stored at ptr (need not match
    /// with value's type) * `flags` specifies alignment, volatility and
    /// pin state
    pub fn new_store(
        &self,
        block: &'_ Block,
        irn_mem: &'_ Node,
        irn_ptr: &'_ Node,
        irn_value: &'_ Node,
        ty: *mut bindings::ir_type,
        flags: bindings::ir_cons_flags::Type,
    ) -> Store {
        let ir_node = unsafe {
            bindings::new_r_Store(
                block.internal_ir_node(),
                irn_mem.internal_ir_node(),
                irn_ptr.internal_ir_node(),
                irn_value.internal_ir_node(),
                ty,
                flags,
            )
        };
        Store::new(ir_node)
    }

    /// Creates a new Sub-node.
    /// * `block` The block.
    /// * `irn_left` left
    /// * `irn_right` right
    pub fn new_sub(&self, block: &'_ Block, irn_left: &'_ Node, irn_right: &'_ Node) -> Sub {
        let ir_node = unsafe {
            bindings::new_r_Sub(
                block.internal_ir_node(),
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
            )
        };
        Sub::new(ir_node)
    }

    /// Creates a new Switch-node.
    /// * `block` The block.
    /// * `irn_selector` selector
    /// * `n_outs` number of outputs (including pn_Switch_default)
    /// * `table` table describing mapping from input values to Proj numbers
    pub fn new_switch(
        &self,
        block: &'_ Block,
        irn_selector: &'_ Node,
        n_outs: ::std::os::raw::c_uint,
        table: *mut bindings::ir_switch_table,
    ) -> Switch {
        let ir_node = unsafe {
            bindings::new_r_Switch(
                block.internal_ir_node(),
                irn_selector.internal_ir_node(),
                n_outs,
                table,
            )
        };
        Switch::new(ir_node)
    }

    /// Creates a new Sync-node.
    /// * `block` The block.
    /// * `in_` additional inputs
    pub fn new_sync(&self, block: &'_ Block, in_: Vec<Node>) -> Sync {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe {
            bindings::new_r_Sync(block.internal_ir_node(), in_.len() as i32, in_.as_ptr())
        };
        Sync::new(ir_node)
    }

    /// Creates a new Tuple-node.
    /// * `block` The block.
    /// * `in_` additional inputs
    pub fn new_tuple(&self, block: &'_ Block, in_: Vec<Node>) -> Tuple {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe {
            bindings::new_r_Tuple(block.internal_ir_node(), in_.len() as i32, in_.as_ptr())
        };
        Tuple::new(ir_node)
    }

    /// Creates a new Unknown-node.
    /// * `mode` mode of the operations result
    pub fn new_unknown(&self, mode: *mut bindings::ir_mode) -> Unknown {
        let ir_node = unsafe { bindings::new_r_Unknown(self.irg, mode) };
        Unknown::new(ir_node)
    }
}

impl Block {
    /// Creates a new Add-node.
    /// * `irn_left` left
    /// * `irn_right` right
    pub fn new_add(&self, irn_left: &'_ Node, irn_right: &'_ Node) -> Add {
        let ir_node = unsafe {
            bindings::new_r_Add(
                self.0,
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
            )
        };
        Add::new(ir_node)
    }

    /// Creates a new Alloc-node.
    /// * `irn_mem` mem
    /// * `irn_size` size
    /// * `alignment` alignment of the memory block (must be a power of 2)
    pub fn new_alloc(
        &self,
        irn_mem: &'_ Node,
        irn_size: &'_ Node,
        alignment: ::std::os::raw::c_uint,
    ) -> Alloc {
        let ir_node = unsafe {
            bindings::new_r_Alloc(
                self.0,
                irn_mem.internal_ir_node(),
                irn_size.internal_ir_node(),
                alignment,
            )
        };
        Alloc::new(ir_node)
    }

    /// Creates a new And-node.
    /// * `irn_left` left
    /// * `irn_right` right
    pub fn new_and(&self, irn_left: &'_ Node, irn_right: &'_ Node) -> And {
        let ir_node = unsafe {
            bindings::new_r_And(
                self.0,
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
            )
        };
        And::new(ir_node)
    }

    /// Creates a new Bitcast-node.
    /// * `irn_op` op
    /// * `mode` mode of the operations result
    pub fn new_bitcast(&self, irn_op: &'_ Node, mode: *mut bindings::ir_mode) -> Bitcast {
        let ir_node = unsafe { bindings::new_r_Bitcast(self.0, irn_op.internal_ir_node(), mode) };
        Bitcast::new(ir_node)
    }

    /// Creates a new Builtin-node.
    /// * `irn_mem` mem
    /// * `in_` additional inputs
    /// * `kind` kind of builtin
    /// * `ty` method type for the builtin call
    pub fn new_builtin(
        &self,
        irn_mem: &'_ Node,
        in_: Vec<Node>,
        kind: bindings::ir_builtin_kind::Type,
        ty: *mut bindings::ir_type,
    ) -> Builtin {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe {
            bindings::new_r_Builtin(
                self.0,
                irn_mem.internal_ir_node(),
                in_.len() as i32,
                in_.as_ptr(),
                kind,
                ty,
            )
        };
        Builtin::new(ir_node)
    }

    /// Creates a new Call-node.
    /// * `irn_mem` mem
    /// * `irn_ptr` ptr
    /// * `in_` additional inputs
    /// * `ty` type of the call (usually type of the called procedure)
    pub fn new_call(
        &self,
        irn_mem: &'_ Node,
        irn_ptr: &'_ Node,
        in_: Vec<Node>,
        ty: *mut bindings::ir_type,
    ) -> Call {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe {
            bindings::new_r_Call(
                self.0,
                irn_mem.internal_ir_node(),
                irn_ptr.internal_ir_node(),
                in_.len() as i32,
                in_.as_ptr(),
                ty,
            )
        };
        Call::new(ir_node)
    }

    /// Creates a new Cmp-node.
    /// * `irn_left` left
    /// * `irn_right` right
    /// * `relation` Comparison relation
    pub fn new_cmp(
        &self,
        irn_left: &'_ Node,
        irn_right: &'_ Node,
        relation: bindings::ir_relation::Type,
    ) -> Cmp {
        let ir_node = unsafe {
            bindings::new_r_Cmp(
                self.0,
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
                relation,
            )
        };
        Cmp::new(ir_node)
    }

    /// Creates a new Cond-node.
    /// * `irn_selector` selector
    pub fn new_cond(&self, irn_selector: &'_ Node) -> Cond {
        let ir_node = unsafe { bindings::new_r_Cond(self.0, irn_selector.internal_ir_node()) };
        Cond::new(ir_node)
    }

    /// Creates a new Confirm-node.
    /// * `irn_value` value
    /// * `irn_bound` bound
    /// * `relation` relation of value to bound
    pub fn new_confirm(
        &self,
        irn_value: &'_ Node,
        irn_bound: &'_ Node,
        relation: bindings::ir_relation::Type,
    ) -> Confirm {
        let ir_node = unsafe {
            bindings::new_r_Confirm(
                self.0,
                irn_value.internal_ir_node(),
                irn_bound.internal_ir_node(),
                relation,
            )
        };
        Confirm::new(ir_node)
    }

    /// Creates a new Conv-node.
    /// * `irn_op` op
    /// * `mode` mode of the operations result
    pub fn new_conv(&self, irn_op: &'_ Node, mode: *mut bindings::ir_mode) -> Conv {
        let ir_node = unsafe { bindings::new_r_Conv(self.0, irn_op.internal_ir_node(), mode) };
        Conv::new(ir_node)
    }

    /// Creates a new CopyB-node.
    /// * `irn_mem` mem
    /// * `irn_dst` dst
    /// * `irn_src` src
    /// * `ty` type of copied data
    /// * `flags` specifies volatility
    pub fn new_copyb(
        &self,
        irn_mem: &'_ Node,
        irn_dst: &'_ Node,
        irn_src: &'_ Node,
        ty: *mut bindings::ir_type,
        flags: bindings::ir_cons_flags::Type,
    ) -> CopyB {
        let ir_node = unsafe {
            bindings::new_r_CopyB(
                self.0,
                irn_mem.internal_ir_node(),
                irn_dst.internal_ir_node(),
                irn_src.internal_ir_node(),
                ty,
                flags,
            )
        };
        CopyB::new(ir_node)
    }

    /// Creates a new Div-node.
    /// * `irn_mem` mem
    /// * `irn_left` left
    /// * `irn_right` right
    /// * `pinned` pinned state
    pub fn new_div(
        &self,
        irn_mem: &'_ Node,
        irn_left: &'_ Node,
        irn_right: &'_ Node,
        pinned: i32,
    ) -> Div {
        let ir_node = unsafe {
            bindings::new_r_Div(
                self.0,
                irn_mem.internal_ir_node(),
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
                pinned,
            )
        };
        Div::new(ir_node)
    }

    /// Creates a new Eor-node.
    /// * `irn_left` left
    /// * `irn_right` right
    pub fn new_eor(&self, irn_left: &'_ Node, irn_right: &'_ Node) -> Eor {
        let ir_node = unsafe {
            bindings::new_r_Eor(
                self.0,
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
            )
        };
        Eor::new(ir_node)
    }

    /// Creates a new Free-node.
    /// * `irn_mem` mem
    /// * `irn_ptr` ptr
    pub fn new_free(&self, irn_mem: &'_ Node, irn_ptr: &'_ Node) -> Free {
        let ir_node = unsafe {
            bindings::new_r_Free(
                self.0,
                irn_mem.internal_ir_node(),
                irn_ptr.internal_ir_node(),
            )
        };
        Free::new(ir_node)
    }

    /// Creates a new IJmp-node.
    /// * `irn_target` target
    pub fn new_ijmp(&self, irn_target: &'_ Node) -> IJmp {
        let ir_node = unsafe { bindings::new_r_IJmp(self.0, irn_target.internal_ir_node()) };
        IJmp::new(ir_node)
    }

    /// Creates a new Jmp-node.
    pub fn new_jmp(&self) -> Jmp {
        let ir_node = unsafe { bindings::new_r_Jmp(self.0) };
        Jmp::new(ir_node)
    }

    /// Creates a new Load-node.
    /// * `irn_mem` mem
    /// * `irn_ptr` ptr
    /// * `mode` mode of the value to be loaded
    /// * `ty` The type of the object which is stored at ptr (need not match
    /// with mode) * `flags` specifies alignment, volatility and pin state
    pub fn new_load(
        &self,
        irn_mem: &'_ Node,
        irn_ptr: &'_ Node,
        mode: *mut bindings::ir_mode,
        ty: *mut bindings::ir_type,
        flags: bindings::ir_cons_flags::Type,
    ) -> Load {
        let ir_node = unsafe {
            bindings::new_r_Load(
                self.0,
                irn_mem.internal_ir_node(),
                irn_ptr.internal_ir_node(),
                mode,
                ty,
                flags,
            )
        };
        Load::new(ir_node)
    }

    /// Creates a new Member-node.
    /// * `irn_ptr` ptr
    /// * `entity` entity which is selected
    pub fn new_member(&self, irn_ptr: &'_ Node, entity: *mut bindings::ir_entity) -> Member {
        let ir_node = unsafe { bindings::new_r_Member(self.0, irn_ptr.internal_ir_node(), entity) };
        Member::new(ir_node)
    }

    /// Creates a new Minus-node.
    /// * `irn_op` op
    pub fn new_minus(&self, irn_op: &'_ Node) -> Minus {
        let ir_node = unsafe { bindings::new_r_Minus(self.0, irn_op.internal_ir_node()) };
        Minus::new(ir_node)
    }

    /// Creates a new Mod-node.
    /// * `irn_mem` mem
    /// * `irn_left` left
    /// * `irn_right` right
    /// * `pinned` pinned state
    pub fn new_mod(
        &self,
        irn_mem: &'_ Node,
        irn_left: &'_ Node,
        irn_right: &'_ Node,
        pinned: i32,
    ) -> Mod {
        let ir_node = unsafe {
            bindings::new_r_Mod(
                self.0,
                irn_mem.internal_ir_node(),
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
                pinned,
            )
        };
        Mod::new(ir_node)
    }

    /// Creates a new Mul-node.
    /// * `irn_left` left
    /// * `irn_right` right
    pub fn new_mul(&self, irn_left: &'_ Node, irn_right: &'_ Node) -> Mul {
        let ir_node = unsafe {
            bindings::new_r_Mul(
                self.0,
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
            )
        };
        Mul::new(ir_node)
    }

    /// Creates a new Mulh-node.
    /// * `irn_left` left
    /// * `irn_right` right
    pub fn new_mulh(&self, irn_left: &'_ Node, irn_right: &'_ Node) -> Mulh {
        let ir_node = unsafe {
            bindings::new_r_Mulh(
                self.0,
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
            )
        };
        Mulh::new(ir_node)
    }

    /// Creates a new Mux-node.
    /// * `irn_sel` sel
    /// * `irn_false` false
    /// * `irn_true` true
    pub fn new_mux(&self, irn_sel: &'_ Node, irn_false: &'_ Node, irn_true: &'_ Node) -> Mux {
        let ir_node = unsafe {
            bindings::new_r_Mux(
                self.0,
                irn_sel.internal_ir_node(),
                irn_false.internal_ir_node(),
                irn_true.internal_ir_node(),
            )
        };
        Mux::new(ir_node)
    }

    /// Creates a new Not-node.
    /// * `irn_op` op
    pub fn new_not(&self, irn_op: &'_ Node) -> Not {
        let ir_node = unsafe { bindings::new_r_Not(self.0, irn_op.internal_ir_node()) };
        Not::new(ir_node)
    }

    /// Creates a new Or-node.
    /// * `irn_left` left
    /// * `irn_right` right
    pub fn new_or(&self, irn_left: &'_ Node, irn_right: &'_ Node) -> Or {
        let ir_node = unsafe {
            bindings::new_r_Or(
                self.0,
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
            )
        };
        Or::new(ir_node)
    }

    /// Creates a new Phi-node.
    /// * `in_` additional inputs
    /// * `mode` mode of the operations result
    pub fn new_phi(&self, in_: Vec<Node>, mode: *mut bindings::ir_mode) -> Phi {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe { bindings::new_r_Phi(self.0, in_.len() as i32, in_.as_ptr(), mode) };
        Phi::new(ir_node)
    }

    /// Creates a new Pin-node.
    /// * `irn_op` op
    pub fn new_pin(&self, irn_op: &'_ Node) -> Pin {
        let ir_node = unsafe { bindings::new_r_Pin(self.0, irn_op.internal_ir_node()) };
        Pin::new(ir_node)
    }

    /// Creates a new Raise-node.
    /// * `irn_mem` mem
    /// * `irn_exo_ptr` exo_ptr
    pub fn new_raise(&self, irn_mem: &'_ Node, irn_exo_ptr: &'_ Node) -> Raise {
        let ir_node = unsafe {
            bindings::new_r_Raise(
                self.0,
                irn_mem.internal_ir_node(),
                irn_exo_ptr.internal_ir_node(),
            )
        };
        Raise::new(ir_node)
    }

    /// Creates a new Return-node.
    /// * `irn_mem` mem
    /// * `in_` additional inputs
    pub fn new_return(&self, irn_mem: &'_ Node, in_: Vec<Node>) -> Return {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe {
            bindings::new_r_Return(
                self.0,
                irn_mem.internal_ir_node(),
                in_.len() as i32,
                in_.as_ptr(),
            )
        };
        Return::new(ir_node)
    }

    /// Creates a new Sel-node.
    /// * `irn_ptr` ptr
    /// * `irn_index` index
    /// * `ty` array type
    pub fn new_sel(
        &self,
        irn_ptr: &'_ Node,
        irn_index: &'_ Node,
        ty: *mut bindings::ir_type,
    ) -> Sel {
        let ir_node = unsafe {
            bindings::new_r_Sel(
                self.0,
                irn_ptr.internal_ir_node(),
                irn_index.internal_ir_node(),
                ty,
            )
        };
        Sel::new(ir_node)
    }

    /// Creates a new Shl-node.
    /// * `irn_left` left
    /// * `irn_right` right
    pub fn new_shl(&self, irn_left: &'_ Node, irn_right: &'_ Node) -> Shl {
        let ir_node = unsafe {
            bindings::new_r_Shl(
                self.0,
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
            )
        };
        Shl::new(ir_node)
    }

    /// Creates a new Shr-node.
    /// * `irn_left` left
    /// * `irn_right` right
    pub fn new_shr(&self, irn_left: &'_ Node, irn_right: &'_ Node) -> Shr {
        let ir_node = unsafe {
            bindings::new_r_Shr(
                self.0,
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
            )
        };
        Shr::new(ir_node)
    }

    /// Creates a new Shrs-node.
    /// * `irn_left` left
    /// * `irn_right` right
    pub fn new_shrs(&self, irn_left: &'_ Node, irn_right: &'_ Node) -> Shrs {
        let ir_node = unsafe {
            bindings::new_r_Shrs(
                self.0,
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
            )
        };
        Shrs::new(ir_node)
    }

    /// Creates a new Store-node.
    /// * `irn_mem` mem
    /// * `irn_ptr` ptr
    /// * `irn_value` value
    /// * `ty` The type of the object which is stored at ptr (need not match
    /// with value's type) * `flags` specifies alignment, volatility and
    /// pin state
    pub fn new_store(
        &self,
        irn_mem: &'_ Node,
        irn_ptr: &'_ Node,
        irn_value: &'_ Node,
        ty: *mut bindings::ir_type,
        flags: bindings::ir_cons_flags::Type,
    ) -> Store {
        let ir_node = unsafe {
            bindings::new_r_Store(
                self.0,
                irn_mem.internal_ir_node(),
                irn_ptr.internal_ir_node(),
                irn_value.internal_ir_node(),
                ty,
                flags,
            )
        };
        Store::new(ir_node)
    }

    /// Creates a new Sub-node.
    /// * `irn_left` left
    /// * `irn_right` right
    pub fn new_sub(&self, irn_left: &'_ Node, irn_right: &'_ Node) -> Sub {
        let ir_node = unsafe {
            bindings::new_r_Sub(
                self.0,
                irn_left.internal_ir_node(),
                irn_right.internal_ir_node(),
            )
        };
        Sub::new(ir_node)
    }

    /// Creates a new Switch-node.
    /// * `irn_selector` selector
    /// * `n_outs` number of outputs (including pn_Switch_default)
    /// * `table` table describing mapping from input values to Proj numbers
    pub fn new_switch(
        &self,
        irn_selector: &'_ Node,
        n_outs: ::std::os::raw::c_uint,
        table: *mut bindings::ir_switch_table,
    ) -> Switch {
        let ir_node = unsafe {
            bindings::new_r_Switch(self.0, irn_selector.internal_ir_node(), n_outs, table)
        };
        Switch::new(ir_node)
    }

    /// Creates a new Sync-node.
    /// * `in_` additional inputs
    pub fn new_sync(&self, in_: Vec<Node>) -> Sync {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe { bindings::new_r_Sync(self.0, in_.len() as i32, in_.as_ptr()) };
        Sync::new(ir_node)
    }

    /// Creates a new Tuple-node.
    /// * `in_` additional inputs
    pub fn new_tuple(&self, in_: Vec<Node>) -> Tuple {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe { bindings::new_r_Tuple(self.0, in_.len() as i32, in_.as_ptr()) };
        Tuple::new(ir_node)
    }
}
