// This file is generated! Do not edit by hand!
// Follow the instructions in the README on how to update this file.

#![allow(dead_code)]
use crate::{
    bindings,
    nodes::{NodeDebug, NodeDebugOpts, NodeTrait},
    types::{Ty, TyTrait},
    Entity, Graph, Mode, Tarval,
};
use std::{collections::HashMap, fmt};
#[derive(Clone, Copy)]
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

#[allow(clippy::wrong_self_convention)]
impl Node {
    pub fn is_add(node: Node) -> bool {
        match node {
            Node::Add(_node) => true,
            _ => false,
        }
    }
    pub fn as_add(node: Node) -> Option<Add> {
        match node {
            Node::Add(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_address(node: Node) -> bool {
        match node {
            Node::Address(_node) => true,
            _ => false,
        }
    }
    pub fn as_address(node: Node) -> Option<Address> {
        match node {
            Node::Address(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_align(node: Node) -> bool {
        match node {
            Node::Align(_node) => true,
            _ => false,
        }
    }
    pub fn as_align(node: Node) -> Option<Align> {
        match node {
            Node::Align(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_alloc(node: Node) -> bool {
        match node {
            Node::Alloc(_node) => true,
            _ => false,
        }
    }
    pub fn as_alloc(node: Node) -> Option<Alloc> {
        match node {
            Node::Alloc(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_anchor(node: Node) -> bool {
        match node {
            Node::Anchor(_node) => true,
            _ => false,
        }
    }
    pub fn as_anchor(node: Node) -> Option<Anchor> {
        match node {
            Node::Anchor(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_and(node: Node) -> bool {
        match node {
            Node::And(_node) => true,
            _ => false,
        }
    }
    pub fn as_and(node: Node) -> Option<And> {
        match node {
            Node::And(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_bad(node: Node) -> bool {
        match node {
            Node::Bad(_node) => true,
            _ => false,
        }
    }
    pub fn as_bad(node: Node) -> Option<Bad> {
        match node {
            Node::Bad(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_bitcast(node: Node) -> bool {
        match node {
            Node::Bitcast(_node) => true,
            _ => false,
        }
    }
    pub fn as_bitcast(node: Node) -> Option<Bitcast> {
        match node {
            Node::Bitcast(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_block(node: Node) -> bool {
        match node {
            Node::Block(_node) => true,
            _ => false,
        }
    }
    pub fn as_block(node: Node) -> Option<Block> {
        match node {
            Node::Block(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_builtin(node: Node) -> bool {
        match node {
            Node::Builtin(_node) => true,
            _ => false,
        }
    }
    pub fn as_builtin(node: Node) -> Option<Builtin> {
        match node {
            Node::Builtin(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_call(node: Node) -> bool {
        match node {
            Node::Call(_node) => true,
            _ => false,
        }
    }
    pub fn as_call(node: Node) -> Option<Call> {
        match node {
            Node::Call(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_cmp(node: Node) -> bool {
        match node {
            Node::Cmp(_node) => true,
            _ => false,
        }
    }
    pub fn as_cmp(node: Node) -> Option<Cmp> {
        match node {
            Node::Cmp(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_cond(node: Node) -> bool {
        match node {
            Node::Cond(_node) => true,
            _ => false,
        }
    }
    pub fn as_cond(node: Node) -> Option<Cond> {
        match node {
            Node::Cond(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_confirm(node: Node) -> bool {
        match node {
            Node::Confirm(_node) => true,
            _ => false,
        }
    }
    pub fn as_confirm(node: Node) -> Option<Confirm> {
        match node {
            Node::Confirm(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_const(node: Node) -> bool {
        match node {
            Node::Const(_node) => true,
            _ => false,
        }
    }
    pub fn as_const(node: Node) -> Option<Const> {
        match node {
            Node::Const(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_conv(node: Node) -> bool {
        match node {
            Node::Conv(_node) => true,
            _ => false,
        }
    }
    pub fn as_conv(node: Node) -> Option<Conv> {
        match node {
            Node::Conv(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_copyb(node: Node) -> bool {
        match node {
            Node::CopyB(_node) => true,
            _ => false,
        }
    }
    pub fn as_copyb(node: Node) -> Option<CopyB> {
        match node {
            Node::CopyB(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_deleted(node: Node) -> bool {
        match node {
            Node::Deleted(_node) => true,
            _ => false,
        }
    }
    pub fn as_deleted(node: Node) -> Option<Deleted> {
        match node {
            Node::Deleted(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_div(node: Node) -> bool {
        match node {
            Node::Div(_node) => true,
            _ => false,
        }
    }
    pub fn as_div(node: Node) -> Option<Div> {
        match node {
            Node::Div(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_dummy(node: Node) -> bool {
        match node {
            Node::Dummy(_node) => true,
            _ => false,
        }
    }
    pub fn as_dummy(node: Node) -> Option<Dummy> {
        match node {
            Node::Dummy(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_end(node: Node) -> bool {
        match node {
            Node::End(_node) => true,
            _ => false,
        }
    }
    pub fn as_end(node: Node) -> Option<End> {
        match node {
            Node::End(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_eor(node: Node) -> bool {
        match node {
            Node::Eor(_node) => true,
            _ => false,
        }
    }
    pub fn as_eor(node: Node) -> Option<Eor> {
        match node {
            Node::Eor(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_free(node: Node) -> bool {
        match node {
            Node::Free(_node) => true,
            _ => false,
        }
    }
    pub fn as_free(node: Node) -> Option<Free> {
        match node {
            Node::Free(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_ijmp(node: Node) -> bool {
        match node {
            Node::IJmp(_node) => true,
            _ => false,
        }
    }
    pub fn as_ijmp(node: Node) -> Option<IJmp> {
        match node {
            Node::IJmp(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_id(node: Node) -> bool {
        match node {
            Node::Id(_node) => true,
            _ => false,
        }
    }
    pub fn as_id(node: Node) -> Option<Id> {
        match node {
            Node::Id(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_jmp(node: Node) -> bool {
        match node {
            Node::Jmp(_node) => true,
            _ => false,
        }
    }
    pub fn as_jmp(node: Node) -> Option<Jmp> {
        match node {
            Node::Jmp(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_load(node: Node) -> bool {
        match node {
            Node::Load(_node) => true,
            _ => false,
        }
    }
    pub fn as_load(node: Node) -> Option<Load> {
        match node {
            Node::Load(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_member(node: Node) -> bool {
        match node {
            Node::Member(_node) => true,
            _ => false,
        }
    }
    pub fn as_member(node: Node) -> Option<Member> {
        match node {
            Node::Member(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_minus(node: Node) -> bool {
        match node {
            Node::Minus(_node) => true,
            _ => false,
        }
    }
    pub fn as_minus(node: Node) -> Option<Minus> {
        match node {
            Node::Minus(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_mod(node: Node) -> bool {
        match node {
            Node::Mod(_node) => true,
            _ => false,
        }
    }
    pub fn as_mod(node: Node) -> Option<Mod> {
        match node {
            Node::Mod(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_mul(node: Node) -> bool {
        match node {
            Node::Mul(_node) => true,
            _ => false,
        }
    }
    pub fn as_mul(node: Node) -> Option<Mul> {
        match node {
            Node::Mul(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_mulh(node: Node) -> bool {
        match node {
            Node::Mulh(_node) => true,
            _ => false,
        }
    }
    pub fn as_mulh(node: Node) -> Option<Mulh> {
        match node {
            Node::Mulh(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_mux(node: Node) -> bool {
        match node {
            Node::Mux(_node) => true,
            _ => false,
        }
    }
    pub fn as_mux(node: Node) -> Option<Mux> {
        match node {
            Node::Mux(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_nomem(node: Node) -> bool {
        match node {
            Node::NoMem(_node) => true,
            _ => false,
        }
    }
    pub fn as_nomem(node: Node) -> Option<NoMem> {
        match node {
            Node::NoMem(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_not(node: Node) -> bool {
        match node {
            Node::Not(_node) => true,
            _ => false,
        }
    }
    pub fn as_not(node: Node) -> Option<Not> {
        match node {
            Node::Not(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_offset(node: Node) -> bool {
        match node {
            Node::Offset(_node) => true,
            _ => false,
        }
    }
    pub fn as_offset(node: Node) -> Option<Offset> {
        match node {
            Node::Offset(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_or(node: Node) -> bool {
        match node {
            Node::Or(_node) => true,
            _ => false,
        }
    }
    pub fn as_or(node: Node) -> Option<Or> {
        match node {
            Node::Or(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_phi(node: Node) -> bool {
        match node {
            Node::Phi(_node) => true,
            _ => false,
        }
    }
    pub fn as_phi(node: Node) -> Option<Phi> {
        match node {
            Node::Phi(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_pin(node: Node) -> bool {
        match node {
            Node::Pin(_node) => true,
            _ => false,
        }
    }
    pub fn as_pin(node: Node) -> Option<Pin> {
        match node {
            Node::Pin(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_proj(node: Node) -> bool {
        match node {
            Node::Proj(_node, _proj_kind) => true,
            _ => false,
        }
    }
    pub fn as_proj(node: Node) -> Option<Proj> {
        match node {
            Node::Proj(node, _proj_kind) => Some(node),
            _ => None,
        }
    }
    pub fn is_raise(node: Node) -> bool {
        match node {
            Node::Raise(_node) => true,
            _ => false,
        }
    }
    pub fn as_raise(node: Node) -> Option<Raise> {
        match node {
            Node::Raise(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_return(node: Node) -> bool {
        match node {
            Node::Return(_node) => true,
            _ => false,
        }
    }
    pub fn as_return(node: Node) -> Option<Return> {
        match node {
            Node::Return(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_sel(node: Node) -> bool {
        match node {
            Node::Sel(_node) => true,
            _ => false,
        }
    }
    pub fn as_sel(node: Node) -> Option<Sel> {
        match node {
            Node::Sel(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_shl(node: Node) -> bool {
        match node {
            Node::Shl(_node) => true,
            _ => false,
        }
    }
    pub fn as_shl(node: Node) -> Option<Shl> {
        match node {
            Node::Shl(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_shr(node: Node) -> bool {
        match node {
            Node::Shr(_node) => true,
            _ => false,
        }
    }
    pub fn as_shr(node: Node) -> Option<Shr> {
        match node {
            Node::Shr(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_shrs(node: Node) -> bool {
        match node {
            Node::Shrs(_node) => true,
            _ => false,
        }
    }
    pub fn as_shrs(node: Node) -> Option<Shrs> {
        match node {
            Node::Shrs(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_size(node: Node) -> bool {
        match node {
            Node::Size(_node) => true,
            _ => false,
        }
    }
    pub fn as_size(node: Node) -> Option<Size> {
        match node {
            Node::Size(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_start(node: Node) -> bool {
        match node {
            Node::Start(_node) => true,
            _ => false,
        }
    }
    pub fn as_start(node: Node) -> Option<Start> {
        match node {
            Node::Start(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_store(node: Node) -> bool {
        match node {
            Node::Store(_node) => true,
            _ => false,
        }
    }
    pub fn as_store(node: Node) -> Option<Store> {
        match node {
            Node::Store(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_sub(node: Node) -> bool {
        match node {
            Node::Sub(_node) => true,
            _ => false,
        }
    }
    pub fn as_sub(node: Node) -> Option<Sub> {
        match node {
            Node::Sub(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_switch(node: Node) -> bool {
        match node {
            Node::Switch(_node) => true,
            _ => false,
        }
    }
    pub fn as_switch(node: Node) -> Option<Switch> {
        match node {
            Node::Switch(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_sync(node: Node) -> bool {
        match node {
            Node::Sync(_node) => true,
            _ => false,
        }
    }
    pub fn as_sync(node: Node) -> Option<Sync> {
        match node {
            Node::Sync(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_tuple(node: Node) -> bool {
        match node {
            Node::Tuple(_node) => true,
            _ => false,
        }
    }
    pub fn as_tuple(node: Node) -> Option<Tuple> {
        match node {
            Node::Tuple(node) => Some(node),
            _ => None,
        }
    }
    pub fn is_unknown(node: Node) -> bool {
        match node {
            Node::Unknown(_node) => true,
            _ => false,
        }
    }
    pub fn as_unknown(node: Node) -> Option<Unknown> {
        match node {
            Node::Unknown(node) => Some(node),
            _ => None,
        }
    }
}
impl NodeDebug for Node {
    fn fmt(&self, f: &mut fmt::Formatter, opts: NodeDebugOpts) -> fmt::Result {
        match self {
            Node::Add(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Address(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Align(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Alloc(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Anchor(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::And(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Bad(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Bitcast(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Block(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Builtin(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Call(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Cmp(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Cond(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Confirm(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Const(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Conv(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::CopyB(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Deleted(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Div(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Dummy(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::End(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Eor(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Free(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::IJmp(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Id(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Jmp(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Load(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Member(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Minus(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Mod(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Mul(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Mulh(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Mux(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::NoMem(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Not(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Offset(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Or(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Phi(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Pin(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Proj(node, _proj_kind) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Raise(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Return(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Sel(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Shl(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Shr(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Shrs(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Size(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Start(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Store(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Sub(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Switch(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Sync(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Tuple(node) => write!(f, "{}", node.debug_fmt().with(opts)),
            Node::Unknown(node) => write!(f, "{}", node.debug_fmt().with(opts)),
        }
    }
}
impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
#[allow(non_camel_case_types)]
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
    Call_TResult_Arg(
        /* arg_idx */ u32,
        /* pred_pred */ Call,
        /* pred */ Proj,
    ),
    /// control flow if operand is "false" or "true"
    Cond_Val(bool, Cond),
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

impl Proj {
    pub fn kind(self) -> ProjKind {
        let pred = self.pred_or_none();
        match pred {
            Some(Node::Alloc(node)) => match self.num() {
                0 => ProjKind::Alloc_M(node),
                1 => ProjKind::Alloc_Res(node),
                _ => ProjKind::Other,
            },
            Some(Node::Builtin(node)) => match self.num() {
                0 => ProjKind::Builtin_M(node),
                _ => ProjKind::Other,
            },
            Some(Node::Call(node)) => match self.num() {
                0 => ProjKind::Call_M(node),
                1 => ProjKind::Call_TResult(node),
                2 => ProjKind::Call_XRegular(node),
                3 => ProjKind::Call_XExcept(node),
                _ => ProjKind::Other,
            },
            Some(Node::Cond(node)) => match self.num() {
                0 => ProjKind::Cond_Val(false, node),
                1 => ProjKind::Cond_Val(true, node),
                _ => ProjKind::Other,
            },
            Some(Node::Div(node)) => match self.num() {
                0 => ProjKind::Div_M(node),
                1 => ProjKind::Div_Res(node),
                2 => ProjKind::Div_XRegular(node),
                3 => ProjKind::Div_XExcept(node),
                _ => ProjKind::Other,
            },
            Some(Node::Load(node)) => match self.num() {
                0 => ProjKind::Load_M(node),
                1 => ProjKind::Load_Res(node),
                2 => ProjKind::Load_XRegular(node),
                3 => ProjKind::Load_XExcept(node),
                _ => ProjKind::Other,
            },
            Some(Node::Mod(node)) => match self.num() {
                0 => ProjKind::Mod_M(node),
                1 => ProjKind::Mod_Res(node),
                2 => ProjKind::Mod_XRegular(node),
                3 => ProjKind::Mod_XExcept(node),
                _ => ProjKind::Other,
            },
            Some(Node::Raise(node)) => match self.num() {
                0 => ProjKind::Raise_M(node),
                1 => ProjKind::Raise_X(node),
                _ => ProjKind::Other,
            },
            Some(Node::Start(node)) => match self.num() {
                0 => ProjKind::Start_M(node),
                1 => ProjKind::Start_PFrameBase(node),
                2 => ProjKind::Start_TArgs(node),
                _ => ProjKind::Other,
            },
            Some(Node::Store(node)) => match self.num() {
                0 => ProjKind::Store_M(node),
                1 => ProjKind::Store_XRegular(node),
                2 => ProjKind::Store_XExcept(node),
                _ => ProjKind::Other,
            },
            Some(Node::Switch(node)) => match self.num() {
                0 => ProjKind::Switch_Default(node),
                _ => ProjKind::Other,
            },
            Some(Node::Proj(proj, ProjKind::Start_TArgs(start))) => {
                ProjKind::Start_TArgs_Arg(self.num(), start, proj)
            }
            Some(Node::Proj(proj, ProjKind::Call_TResult(call))) => {
                ProjKind::Call_TResult_Arg(self.num(), call, proj)
            }
            _ => ProjKind::Other,
        }
    }
}

type NodeFactoryFn = fn(*mut bindings::ir_node) -> Node;
pub struct NodeFactory(HashMap<u32, NodeFactoryFn>);
#[allow(clippy::new_without_default_derive)]
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
        let f = self.0[&op_code];
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
        let proj = Proj(ir_node);
        Node::Proj(proj, proj.kind())
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
    #[allow(clippy::let_and_return)]
    pub fn left(self) -> Node {
        let unwrapped = unsafe { bindings::get_Add_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    #[allow(clippy::let_and_return)]
    pub fn set_left(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Add_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    #[allow(clippy::let_and_return)]
    pub fn right(self) -> Node {
        let unwrapped = unsafe { bindings::get_Add_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    #[allow(clippy::let_and_return)]
    pub fn set_right(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Add_right(self.0, unwrapped);
        }
    }
}

impl From<Add> for Node {
    fn from(node: Add) -> Node {
        Node::Add(node)
    }
}

impl NodeTrait for Add {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Add {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Add {}", self.node_id())
    }
}
impl fmt::Debug for Add {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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

impl From<Address> for Node {
    fn from(node: Address) -> Node {
        Node::Address(node)
    }
}

impl NodeTrait for Address {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl fmt::Debug for Address {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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

impl From<Align> for Node {
    fn from(node: Align) -> Node {
        Node::Align(node)
    }
}

impl NodeTrait for Align {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Align {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Align {}", self.node_id())
    }
}
impl fmt::Debug for Align {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn mem(self) -> Node {
        let unwrapped = unsafe { bindings::get_Alloc_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    #[allow(clippy::let_and_return)]
    pub fn set_mem(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Alloc_mem(self.0, unwrapped);
        }
    }

    /// Gets size of the block in bytes.
    #[allow(clippy::let_and_return)]
    pub fn size(self) -> Node {
        let unwrapped = unsafe { bindings::get_Alloc_size(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets size of the block in bytes.
    #[allow(clippy::let_and_return)]
    pub fn set_size(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Alloc_size(self.0, unwrapped);
        }
    }

    /// Gets alignment of the memory block (must be a power of 2).
    #[allow(clippy::let_and_return)]
    pub fn alignment(self) -> ::std::os::raw::c_uint {
        let unwrapped = unsafe { bindings::get_Alloc_alignment(self.0) };
        unwrapped
    }

    /// Sets alignment of the memory block (must be a power of 2).
    #[allow(clippy::let_and_return)]
    pub fn set_alignment(self, val: ::std::os::raw::c_uint) {
        let unwrapped = val;
        unsafe {
            bindings::set_Alloc_alignment(self.0, unwrapped);
        }
    }

    /// memory result.
    pub fn new_proj_m(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::M, 0) })
    }

    /// pointer to newly allocated memory.
    pub fn new_proj_res(self, mode: Mode) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, mode.libfirm_mode(), 1) })
    }

    /// memory result.
    pub fn out_proj_m(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Alloc_M(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// pointer to newly allocated memory.
    pub fn out_proj_res(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Alloc_Res(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }
}

impl From<Alloc> for Node {
    fn from(node: Alloc) -> Node {
        Node::Alloc(node)
    }
}

impl NodeTrait for Alloc {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Alloc {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Alloc {}", self.node_id())
    }
}
impl fmt::Debug for Alloc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn end_block(self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_end_block(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets block the end node belongs to.
    #[allow(clippy::let_and_return)]
    pub fn set_end_block(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Anchor_end_block(self.0, unwrapped);
        }
    }

    /// Gets block the start node belongs to.
    #[allow(clippy::let_and_return)]
    pub fn start_block(self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_start_block(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets block the start node belongs to.
    #[allow(clippy::let_and_return)]
    pub fn set_start_block(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Anchor_start_block(self.0, unwrapped);
        }
    }

    /// Gets end node of this ir_graph.
    #[allow(clippy::let_and_return)]
    pub fn end(self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_end(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets end node of this ir_graph.
    #[allow(clippy::let_and_return)]
    pub fn set_end(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Anchor_end(self.0, unwrapped);
        }
    }

    /// Gets start node of this ir_graph.
    #[allow(clippy::let_and_return)]
    pub fn start(self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_start(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets start node of this ir_graph.
    #[allow(clippy::let_and_return)]
    pub fn set_start(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Anchor_start(self.0, unwrapped);
        }
    }

    /// Gets frame of this ir_graph.
    #[allow(clippy::let_and_return)]
    pub fn frame(self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_frame(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets frame of this ir_graph.
    #[allow(clippy::let_and_return)]
    pub fn set_frame(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Anchor_frame(self.0, unwrapped);
        }
    }

    /// Gets initial memory of this ir_graph.
    #[allow(clippy::let_and_return)]
    pub fn initial_mem(self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_initial_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets initial memory of this ir_graph.
    #[allow(clippy::let_and_return)]
    pub fn set_initial_mem(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Anchor_initial_mem(self.0, unwrapped);
        }
    }

    /// Gets argument proj of the start node.
    #[allow(clippy::let_and_return)]
    pub fn args(self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_args(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets argument proj of the start node.
    #[allow(clippy::let_and_return)]
    pub fn set_args(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Anchor_args(self.0, unwrapped);
        }
    }

    /// Gets the only NoMem node of this ir_graph.
    #[allow(clippy::let_and_return)]
    pub fn no_mem(self) -> Node {
        let unwrapped = unsafe { bindings::get_Anchor_no_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets the only NoMem node of this ir_graph.
    #[allow(clippy::let_and_return)]
    pub fn set_no_mem(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Anchor_no_mem(self.0, unwrapped);
        }
    }
}

impl From<Anchor> for Node {
    fn from(node: Anchor) -> Node {
        Node::Anchor(node)
    }
}

impl NodeTrait for Anchor {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Anchor {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Anchor {}", self.node_id())
    }
}
impl fmt::Debug for Anchor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn left(self) -> Node {
        let unwrapped = unsafe { bindings::get_And_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    #[allow(clippy::let_and_return)]
    pub fn set_left(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_And_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    #[allow(clippy::let_and_return)]
    pub fn right(self) -> Node {
        let unwrapped = unsafe { bindings::get_And_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    #[allow(clippy::let_and_return)]
    pub fn set_right(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_And_right(self.0, unwrapped);
        }
    }
}

impl From<And> for Node {
    fn from(node: And) -> Node {
        Node::And(node)
    }
}

impl NodeTrait for And {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for And {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "And {}", self.node_id())
    }
}
impl fmt::Debug for And {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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

impl From<Bad> for Node {
    fn from(node: Bad) -> Node {
        Node::Bad(node)
    }
}

impl NodeTrait for Bad {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Bad {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Bad {}", self.node_id())
    }
}
impl fmt::Debug for Bad {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn op(self) -> Node {
        let unwrapped = unsafe { bindings::get_Bitcast_op(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets operand.
    #[allow(clippy::let_and_return)]
    pub fn set_op(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Bitcast_op(self.0, unwrapped);
        }
    }
}

impl From<Bitcast> for Node {
    fn from(node: Bitcast) -> Node {
        Node::Bitcast(node)
    }
}

impl NodeTrait for Bitcast {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Bitcast {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Bitcast {}", self.node_id())
    }
}
impl fmt::Debug for Bitcast {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
    }
}
/// A basic block
#[derive(Clone, Copy, Eq)]
pub struct Block(*mut bindings::ir_node);

impl Block {
    pub(crate) fn new(ir_node: *mut bindings::ir_node) -> Self {
        if unsafe { bindings::is_Block(ir_node) } == 0 {
            panic!("given ir_node is not a Block");
        }
        Block(ir_node)
    }

    /// Gets entity representing this block.
    #[allow(clippy::let_and_return)]
    pub fn entity(self) -> Entity {
        let unwrapped = unsafe { bindings::get_Block_entity(self.0) };
        unwrapped.into()
    }

    /// Sets entity representing this block.
    #[allow(clippy::let_and_return)]
    pub fn set_entity(self, val: Entity) {
        let unwrapped = val.into();
        unsafe {
            bindings::set_Block_entity(self.0, unwrapped);
        }
    }
}

impl From<Block> for Node {
    fn from(node: Block) -> Node {
        Node::Block(node)
    }
}

impl NodeTrait for Block {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Block {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Block {}", self.node_id())
    }
}
impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn mem(self) -> Node {
        let unwrapped = unsafe { bindings::get_Builtin_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    #[allow(clippy::let_and_return)]
    pub fn set_mem(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Builtin_mem(self.0, unwrapped);
        }
    }

    /// Gets kind of builtin.
    #[allow(clippy::let_and_return)]
    pub fn kind(self) -> bindings::ir_builtin_kind::Type {
        let unwrapped = unsafe { bindings::get_Builtin_kind(self.0) };
        unwrapped
    }

    /// Sets kind of builtin.
    #[allow(clippy::let_and_return)]
    pub fn set_kind(self, val: bindings::ir_builtin_kind::Type) {
        let unwrapped = val;
        unsafe {
            bindings::set_Builtin_kind(self.0, unwrapped);
        }
    }

    /// Gets method type for the builtin call.
    #[allow(clippy::let_and_return)]
    pub fn ty(self) -> Ty {
        let unwrapped = unsafe { bindings::get_Builtin_type(self.0) };
        Ty::from_ir_type(unwrapped)
    }

    /// Sets method type for the builtin call.
    #[allow(clippy::let_and_return)]
    pub fn set_ty(self, val: Ty) {
        let unwrapped = val.ir_type();
        unsafe {
            bindings::set_Builtin_type(self.0, unwrapped);
        }
    }

    /// memory result.
    pub fn new_proj_m(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::M, 0) })
    }

    /// memory result.
    pub fn out_proj_m(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Builtin_M(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }
}

impl From<Builtin> for Node {
    fn from(node: Builtin) -> Node {
        Node::Builtin(node)
    }
}

impl NodeTrait for Builtin {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Builtin {}", self.node_id())
    }
}
impl fmt::Debug for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn mem(self) -> Node {
        let unwrapped = unsafe { bindings::get_Call_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    #[allow(clippy::let_and_return)]
    pub fn set_mem(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Call_mem(self.0, unwrapped);
        }
    }

    /// Gets pointer to called code.
    #[allow(clippy::let_and_return)]
    pub fn ptr(self) -> Node {
        let unwrapped = unsafe { bindings::get_Call_ptr(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets pointer to called code.
    #[allow(clippy::let_and_return)]
    pub fn set_ptr(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Call_ptr(self.0, unwrapped);
        }
    }

    /// Gets type of the call (usually type of the called procedure).
    #[allow(clippy::let_and_return)]
    pub fn ty(self) -> Ty {
        let unwrapped = unsafe { bindings::get_Call_type(self.0) };
        Ty::from_ir_type(unwrapped)
    }

    /// Sets type of the call (usually type of the called procedure).
    #[allow(clippy::let_and_return)]
    pub fn set_ty(self, val: Ty) {
        let unwrapped = val.ir_type();
        unsafe {
            bindings::set_Call_type(self.0, unwrapped);
        }
    }

    /// memory result.
    pub fn new_proj_m(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::M, 0) })
    }

    /// tuple containing all results.
    pub fn new_proj_t_result(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::T, 1) })
    }

    /// control flow when no exception occurs.
    pub fn new_proj_x_regular(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 2) })
    }

    /// control flow when exception occurred.
    pub fn new_proj_x_except(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 3) })
    }

    /// memory result.
    pub fn out_proj_m(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Call_M(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// tuple containing all results.
    pub fn out_proj_t_result(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Call_TResult(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow when no exception occurs.
    pub fn out_proj_x_regular(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Call_XRegular(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow when exception occurred.
    pub fn out_proj_x_except(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Call_XExcept(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }
}

impl From<Call> for Node {
    fn from(node: Call) -> Node {
        Node::Call(node)
    }
}

impl NodeTrait for Call {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl fmt::Debug for Call {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn left(self) -> Node {
        let unwrapped = unsafe { bindings::get_Cmp_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    #[allow(clippy::let_and_return)]
    pub fn set_left(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Cmp_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    #[allow(clippy::let_and_return)]
    pub fn right(self) -> Node {
        let unwrapped = unsafe { bindings::get_Cmp_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    #[allow(clippy::let_and_return)]
    pub fn set_right(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Cmp_right(self.0, unwrapped);
        }
    }

    /// Gets Comparison relation.
    #[allow(clippy::let_and_return)]
    pub fn relation(self) -> bindings::ir_relation::Type {
        let unwrapped = unsafe { bindings::get_Cmp_relation(self.0) };
        unwrapped
    }

    /// Sets Comparison relation.
    #[allow(clippy::let_and_return)]
    pub fn set_relation(self, val: bindings::ir_relation::Type) {
        let unwrapped = val;
        unsafe {
            bindings::set_Cmp_relation(self.0, unwrapped);
        }
    }
}

impl From<Cmp> for Node {
    fn from(node: Cmp) -> Node {
        Node::Cmp(node)
    }
}

impl NodeTrait for Cmp {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Cmp {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Cmp {}", self.node_id())
    }
}
impl fmt::Debug for Cmp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn selector(self) -> Node {
        let unwrapped = unsafe { bindings::get_Cond_selector(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets condition parameter.
    #[allow(clippy::let_and_return)]
    pub fn set_selector(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Cond_selector(self.0, unwrapped);
        }
    }

    /// Gets can indicate the most likely jump.
    #[allow(clippy::let_and_return)]
    pub fn jmp_pred(self) -> bindings::cond_jmp_predicate::Type {
        let unwrapped = unsafe { bindings::get_Cond_jmp_pred(self.0) };
        unwrapped
    }

    /// Sets can indicate the most likely jump.
    #[allow(clippy::let_and_return)]
    pub fn set_jmp_pred(self, val: bindings::cond_jmp_predicate::Type) {
        let unwrapped = val;
        unsafe {
            bindings::set_Cond_jmp_pred(self.0, unwrapped);
        }
    }

    /// control flow if operand is "false".
    pub fn new_proj_false(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 0) })
    }

    /// control flow if operand is "true".
    pub fn new_proj_true(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 1) })
    }

    /// control flow if operand is "false".
    pub fn out_proj_false(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Cond_Val(false, _)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow if operand is "true".
    pub fn out_proj_true(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Cond_Val(true, _)) = out_node {
                return Some(proj);
            }
        }
        None
    }
}

impl From<Cond> for Node {
    fn from(node: Cond) -> Node {
        Node::Cond(node)
    }
}

impl NodeTrait for Cond {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Cond {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Cond {}", self.node_id())
    }
}
impl fmt::Debug for Cond {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn value(self) -> Node {
        let unwrapped = unsafe { bindings::get_Confirm_value(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets value to express a constraint for.
    #[allow(clippy::let_and_return)]
    pub fn set_value(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Confirm_value(self.0, unwrapped);
        }
    }

    /// Gets value to compare against.
    #[allow(clippy::let_and_return)]
    pub fn bound(self) -> Node {
        let unwrapped = unsafe { bindings::get_Confirm_bound(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets value to compare against.
    #[allow(clippy::let_and_return)]
    pub fn set_bound(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Confirm_bound(self.0, unwrapped);
        }
    }

    /// Gets relation of value to bound.
    #[allow(clippy::let_and_return)]
    pub fn relation(self) -> bindings::ir_relation::Type {
        let unwrapped = unsafe { bindings::get_Confirm_relation(self.0) };
        unwrapped
    }

    /// Sets relation of value to bound.
    #[allow(clippy::let_and_return)]
    pub fn set_relation(self, val: bindings::ir_relation::Type) {
        let unwrapped = val;
        unsafe {
            bindings::set_Confirm_relation(self.0, unwrapped);
        }
    }
}

impl From<Confirm> for Node {
    fn from(node: Confirm) -> Node {
        Node::Confirm(node)
    }
}

impl NodeTrait for Confirm {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Confirm {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Confirm {}", self.node_id())
    }
}
impl fmt::Debug for Confirm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn tarval(self) -> Tarval {
        let unwrapped = unsafe { bindings::get_Const_tarval(self.0) };
        unwrapped.into()
    }

    /// Sets constant value (a tarval object).
    #[allow(clippy::let_and_return)]
    pub fn set_tarval(self, val: Tarval) {
        let unwrapped = val.into();
        unsafe {
            bindings::set_Const_tarval(self.0, unwrapped);
        }
    }
}

impl From<Const> for Node {
    fn from(node: Const) -> Node {
        Node::Const(node)
    }
}

impl NodeTrait for Const {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl fmt::Debug for Const {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn op(self) -> Node {
        let unwrapped = unsafe { bindings::get_Conv_op(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets operand.
    #[allow(clippy::let_and_return)]
    pub fn set_op(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Conv_op(self.0, unwrapped);
        }
    }
}

impl From<Conv> for Node {
    fn from(node: Conv) -> Node {
        Node::Conv(node)
    }
}

impl NodeTrait for Conv {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Conv {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Conv {}", self.node_id())
    }
}
impl fmt::Debug for Conv {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn mem(self) -> Node {
        let unwrapped = unsafe { bindings::get_CopyB_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    #[allow(clippy::let_and_return)]
    pub fn set_mem(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_CopyB_mem(self.0, unwrapped);
        }
    }

    /// Gets destination address.
    #[allow(clippy::let_and_return)]
    pub fn dst(self) -> Node {
        let unwrapped = unsafe { bindings::get_CopyB_dst(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets destination address.
    #[allow(clippy::let_and_return)]
    pub fn set_dst(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_CopyB_dst(self.0, unwrapped);
        }
    }

    /// Gets source address.
    #[allow(clippy::let_and_return)]
    pub fn src(self) -> Node {
        let unwrapped = unsafe { bindings::get_CopyB_src(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets source address.
    #[allow(clippy::let_and_return)]
    pub fn set_src(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_CopyB_src(self.0, unwrapped);
        }
    }

    /// Gets type of copied data.
    #[allow(clippy::let_and_return)]
    pub fn ty(self) -> Ty {
        let unwrapped = unsafe { bindings::get_CopyB_type(self.0) };
        Ty::from_ir_type(unwrapped)
    }

    /// Sets type of copied data.
    #[allow(clippy::let_and_return)]
    pub fn set_ty(self, val: Ty) {
        let unwrapped = val.ir_type();
        unsafe {
            bindings::set_CopyB_type(self.0, unwrapped);
        }
    }

    /// Gets volatile CopyB nodes have a visible side-effect and may not be
    /// optimized.
    #[allow(clippy::let_and_return)]
    pub fn volatility(self) -> bindings::ir_volatility::Type {
        let unwrapped = unsafe { bindings::get_CopyB_volatility(self.0) };
        unwrapped
    }

    /// Sets volatile CopyB nodes have a visible side-effect and may not be
    /// optimized.
    #[allow(clippy::let_and_return)]
    pub fn set_volatility(self, val: bindings::ir_volatility::Type) {
        let unwrapped = val;
        unsafe {
            bindings::set_CopyB_volatility(self.0, unwrapped);
        }
    }
}

impl From<CopyB> for Node {
    fn from(node: CopyB) -> Node {
        Node::CopyB(node)
    }
}

impl NodeTrait for CopyB {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for CopyB {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "CopyB {}", self.node_id())
    }
}
impl fmt::Debug for CopyB {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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

impl From<Deleted> for Node {
    fn from(node: Deleted) -> Node {
        Node::Deleted(node)
    }
}

impl NodeTrait for Deleted {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Deleted {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Deleted {}", self.node_id())
    }
}
impl fmt::Debug for Deleted {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn mem(self) -> Node {
        let unwrapped = unsafe { bindings::get_Div_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    #[allow(clippy::let_and_return)]
    pub fn set_mem(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Div_mem(self.0, unwrapped);
        }
    }

    /// Gets first operand.
    #[allow(clippy::let_and_return)]
    pub fn left(self) -> Node {
        let unwrapped = unsafe { bindings::get_Div_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    #[allow(clippy::let_and_return)]
    pub fn set_left(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Div_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    #[allow(clippy::let_and_return)]
    pub fn right(self) -> Node {
        let unwrapped = unsafe { bindings::get_Div_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    #[allow(clippy::let_and_return)]
    pub fn set_right(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Div_right(self.0, unwrapped);
        }
    }

    /// Gets mode of the result value.
    #[allow(clippy::let_and_return)]
    pub fn resmode(self) -> Mode {
        let unwrapped = unsafe { bindings::get_Div_resmode(self.0) };
        Mode::from_libfirm(unwrapped)
    }

    /// Sets mode of the result value.
    #[allow(clippy::let_and_return)]
    pub fn set_resmode(self, val: Mode) {
        let unwrapped = val.libfirm_mode();
        unsafe {
            bindings::set_Div_resmode(self.0, unwrapped);
        }
    }

    /// Gets Set when division remainder is known to be zero.
    #[allow(clippy::let_and_return)]
    pub fn no_remainder(self) -> i32 {
        let unwrapped = unsafe { bindings::get_Div_no_remainder(self.0) };
        unwrapped
    }

    /// Sets Set when division remainder is known to be zero.
    #[allow(clippy::let_and_return)]
    pub fn set_no_remainder(self, val: i32) {
        let unwrapped = val;
        unsafe {
            bindings::set_Div_no_remainder(self.0, unwrapped);
        }
    }

    /// memory result.
    pub fn new_proj_m(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::M, 0) })
    }

    /// result of computation.
    pub fn new_proj_res(self, mode: Mode) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, mode.libfirm_mode(), 1) })
    }

    /// control flow when no exception occurs.
    pub fn new_proj_x_regular(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 2) })
    }

    /// control flow when exception occurred.
    pub fn new_proj_x_except(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 3) })
    }

    /// memory result.
    pub fn out_proj_m(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Div_M(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// result of computation.
    pub fn out_proj_res(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Div_Res(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow when no exception occurs.
    pub fn out_proj_x_regular(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Div_XRegular(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow when exception occurred.
    pub fn out_proj_x_except(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Div_XExcept(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }
}

impl From<Div> for Node {
    fn from(node: Div) -> Node {
        Node::Div(node)
    }
}

impl NodeTrait for Div {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Div {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Div {}", self.node_id())
    }
}
impl fmt::Debug for Div {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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

impl From<Dummy> for Node {
    fn from(node: Dummy) -> Node {
        Node::Dummy(node)
    }
}

impl NodeTrait for Dummy {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Dummy {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Dummy {}", self.node_id())
    }
}
impl fmt::Debug for Dummy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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

impl From<End> for Node {
    fn from(node: End) -> Node {
        Node::End(node)
    }
}

impl NodeTrait for End {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for End {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "End {}", self.node_id())
    }
}
impl fmt::Debug for End {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn left(self) -> Node {
        let unwrapped = unsafe { bindings::get_Eor_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    #[allow(clippy::let_and_return)]
    pub fn set_left(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Eor_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    #[allow(clippy::let_and_return)]
    pub fn right(self) -> Node {
        let unwrapped = unsafe { bindings::get_Eor_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    #[allow(clippy::let_and_return)]
    pub fn set_right(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Eor_right(self.0, unwrapped);
        }
    }
}

impl From<Eor> for Node {
    fn from(node: Eor) -> Node {
        Node::Eor(node)
    }
}

impl NodeTrait for Eor {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Eor {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Eor {}", self.node_id())
    }
}
impl fmt::Debug for Eor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn mem(self) -> Node {
        let unwrapped = unsafe { bindings::get_Free_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    #[allow(clippy::let_and_return)]
    pub fn set_mem(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Free_mem(self.0, unwrapped);
        }
    }

    /// Gets pointer to the object to free.
    #[allow(clippy::let_and_return)]
    pub fn ptr(self) -> Node {
        let unwrapped = unsafe { bindings::get_Free_ptr(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets pointer to the object to free.
    #[allow(clippy::let_and_return)]
    pub fn set_ptr(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Free_ptr(self.0, unwrapped);
        }
    }
}

impl From<Free> for Node {
    fn from(node: Free) -> Node {
        Node::Free(node)
    }
}

impl NodeTrait for Free {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Free {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Free {}", self.node_id())
    }
}
impl fmt::Debug for Free {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn target(self) -> Node {
        let unwrapped = unsafe { bindings::get_IJmp_target(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets target address of the jump.
    #[allow(clippy::let_and_return)]
    pub fn set_target(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_IJmp_target(self.0, unwrapped);
        }
    }
}

impl From<IJmp> for Node {
    fn from(node: IJmp) -> Node {
        Node::IJmp(node)
    }
}

impl NodeTrait for IJmp {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for IJmp {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "IJmp {}", self.node_id())
    }
}
impl fmt::Debug for IJmp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn pred(self) -> Node {
        let unwrapped = unsafe { bindings::get_Id_pred(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets the value which is returned unchanged.
    #[allow(clippy::let_and_return)]
    pub fn set_pred(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Id_pred(self.0, unwrapped);
        }
    }
}

impl From<Id> for Node {
    fn from(node: Id) -> Node {
        Node::Id(node)
    }
}

impl NodeTrait for Id {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Id {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Id {}", self.node_id())
    }
}
impl fmt::Debug for Id {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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

impl From<Jmp> for Node {
    fn from(node: Jmp) -> Node {
        Node::Jmp(node)
    }
}

impl NodeTrait for Jmp {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Jmp {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Jmp {}", self.node_id())
    }
}
impl fmt::Debug for Jmp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn mem(self) -> Node {
        let unwrapped = unsafe { bindings::get_Load_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    #[allow(clippy::let_and_return)]
    pub fn set_mem(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Load_mem(self.0, unwrapped);
        }
    }

    /// Gets address to load from.
    #[allow(clippy::let_and_return)]
    pub fn ptr(self) -> Node {
        let unwrapped = unsafe { bindings::get_Load_ptr(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets address to load from.
    #[allow(clippy::let_and_return)]
    pub fn set_ptr(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Load_ptr(self.0, unwrapped);
        }
    }

    /// Gets mode of the value to be loaded.
    #[allow(clippy::let_and_return)]
    pub fn mode(self) -> Mode {
        let unwrapped = unsafe { bindings::get_Load_mode(self.0) };
        Mode::from_libfirm(unwrapped)
    }

    /// Sets mode of the value to be loaded.
    #[allow(clippy::let_and_return)]
    pub fn set_mode(self, val: Mode) {
        let unwrapped = val.libfirm_mode();
        unsafe {
            bindings::set_Load_mode(self.0, unwrapped);
        }
    }

    /// Gets The type of the object which is stored at ptr (need not match with
    /// mode).
    #[allow(clippy::let_and_return)]
    pub fn ty(self) -> Ty {
        let unwrapped = unsafe { bindings::get_Load_type(self.0) };
        Ty::from_ir_type(unwrapped)
    }

    /// Sets The type of the object which is stored at ptr (need not match with
    /// mode).
    #[allow(clippy::let_and_return)]
    pub fn set_ty(self, val: Ty) {
        let unwrapped = val.ir_type();
        unsafe {
            bindings::set_Load_type(self.0, unwrapped);
        }
    }

    /// Gets volatile loads are a visible side-effect and may not be optimized.
    #[allow(clippy::let_and_return)]
    pub fn volatility(self) -> bindings::ir_volatility::Type {
        let unwrapped = unsafe { bindings::get_Load_volatility(self.0) };
        unwrapped
    }

    /// Sets volatile loads are a visible side-effect and may not be optimized.
    #[allow(clippy::let_and_return)]
    pub fn set_volatility(self, val: bindings::ir_volatility::Type) {
        let unwrapped = val;
        unsafe {
            bindings::set_Load_volatility(self.0, unwrapped);
        }
    }

    /// Gets pointers to unaligned loads don't need to respect the
    /// load-mode/type alignments.
    #[allow(clippy::let_and_return)]
    pub fn unaligned(self) -> bindings::ir_align::Type {
        let unwrapped = unsafe { bindings::get_Load_unaligned(self.0) };
        unwrapped
    }

    /// Sets pointers to unaligned loads don't need to respect the
    /// load-mode/type alignments.
    #[allow(clippy::let_and_return)]
    pub fn set_unaligned(self, val: bindings::ir_align::Type) {
        let unwrapped = val;
        unsafe {
            bindings::set_Load_unaligned(self.0, unwrapped);
        }
    }

    /// memory result.
    pub fn new_proj_m(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::M, 0) })
    }

    /// result of load operation.
    pub fn new_proj_res(self, mode: Mode) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, mode.libfirm_mode(), 1) })
    }

    /// control flow when no exception occurs.
    pub fn new_proj_x_regular(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 2) })
    }

    /// control flow when exception occurred.
    pub fn new_proj_x_except(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 3) })
    }

    /// memory result.
    pub fn out_proj_m(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Load_M(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// result of load operation.
    pub fn out_proj_res(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Load_Res(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow when no exception occurs.
    pub fn out_proj_x_regular(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Load_XRegular(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow when exception occurred.
    pub fn out_proj_x_except(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Load_XExcept(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }
}

impl From<Load> for Node {
    fn from(node: Load) -> Node {
        Node::Load(node)
    }
}

impl NodeTrait for Load {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl fmt::Debug for Load {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn ptr(self) -> Node {
        let unwrapped = unsafe { bindings::get_Member_ptr(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets pointer to object to select from.
    #[allow(clippy::let_and_return)]
    pub fn set_ptr(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Member_ptr(self.0, unwrapped);
        }
    }

    /// Gets entity which is selected.
    #[allow(clippy::let_and_return)]
    pub fn entity(self) -> Entity {
        let unwrapped = unsafe { bindings::get_Member_entity(self.0) };
        unwrapped.into()
    }

    /// Sets entity which is selected.
    #[allow(clippy::let_and_return)]
    pub fn set_entity(self, val: Entity) {
        let unwrapped = val.into();
        unsafe {
            bindings::set_Member_entity(self.0, unwrapped);
        }
    }
}

impl From<Member> for Node {
    fn from(node: Member) -> Node {
        Node::Member(node)
    }
}

impl NodeTrait for Member {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl fmt::Debug for Member {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn op(self) -> Node {
        let unwrapped = unsafe { bindings::get_Minus_op(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets operand.
    #[allow(clippy::let_and_return)]
    pub fn set_op(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Minus_op(self.0, unwrapped);
        }
    }
}

impl From<Minus> for Node {
    fn from(node: Minus) -> Node {
        Node::Minus(node)
    }
}

impl NodeTrait for Minus {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Minus {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Minus {}", self.node_id())
    }
}
impl fmt::Debug for Minus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn mem(self) -> Node {
        let unwrapped = unsafe { bindings::get_Mod_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    #[allow(clippy::let_and_return)]
    pub fn set_mem(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Mod_mem(self.0, unwrapped);
        }
    }

    /// Gets first operand.
    #[allow(clippy::let_and_return)]
    pub fn left(self) -> Node {
        let unwrapped = unsafe { bindings::get_Mod_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    #[allow(clippy::let_and_return)]
    pub fn set_left(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Mod_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    #[allow(clippy::let_and_return)]
    pub fn right(self) -> Node {
        let unwrapped = unsafe { bindings::get_Mod_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    #[allow(clippy::let_and_return)]
    pub fn set_right(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Mod_right(self.0, unwrapped);
        }
    }

    /// Gets mode of the result.
    #[allow(clippy::let_and_return)]
    pub fn resmode(self) -> Mode {
        let unwrapped = unsafe { bindings::get_Mod_resmode(self.0) };
        Mode::from_libfirm(unwrapped)
    }

    /// Sets mode of the result.
    #[allow(clippy::let_and_return)]
    pub fn set_resmode(self, val: Mode) {
        let unwrapped = val.libfirm_mode();
        unsafe {
            bindings::set_Mod_resmode(self.0, unwrapped);
        }
    }

    /// memory result.
    pub fn new_proj_m(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::M, 0) })
    }

    /// result of computation.
    pub fn new_proj_res(self, mode: Mode) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, mode.libfirm_mode(), 1) })
    }

    /// control flow when no exception occurs.
    pub fn new_proj_x_regular(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 2) })
    }

    /// control flow when exception occurred.
    pub fn new_proj_x_except(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 3) })
    }

    /// memory result.
    pub fn out_proj_m(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Mod_M(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// result of computation.
    pub fn out_proj_res(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Mod_Res(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow when no exception occurs.
    pub fn out_proj_x_regular(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Mod_XRegular(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow when exception occurred.
    pub fn out_proj_x_except(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Mod_XExcept(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }
}

impl From<Mod> for Node {
    fn from(node: Mod) -> Node {
        Node::Mod(node)
    }
}

impl NodeTrait for Mod {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Mod {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Mod {}", self.node_id())
    }
}
impl fmt::Debug for Mod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn left(self) -> Node {
        let unwrapped = unsafe { bindings::get_Mul_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    #[allow(clippy::let_and_return)]
    pub fn set_left(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Mul_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    #[allow(clippy::let_and_return)]
    pub fn right(self) -> Node {
        let unwrapped = unsafe { bindings::get_Mul_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    #[allow(clippy::let_and_return)]
    pub fn set_right(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Mul_right(self.0, unwrapped);
        }
    }
}

impl From<Mul> for Node {
    fn from(node: Mul) -> Node {
        Node::Mul(node)
    }
}

impl NodeTrait for Mul {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Mul {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Mul {}", self.node_id())
    }
}
impl fmt::Debug for Mul {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn left(self) -> Node {
        let unwrapped = unsafe { bindings::get_Mulh_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    #[allow(clippy::let_and_return)]
    pub fn set_left(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Mulh_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    #[allow(clippy::let_and_return)]
    pub fn right(self) -> Node {
        let unwrapped = unsafe { bindings::get_Mulh_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    #[allow(clippy::let_and_return)]
    pub fn set_right(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Mulh_right(self.0, unwrapped);
        }
    }
}

impl From<Mulh> for Node {
    fn from(node: Mulh) -> Node {
        Node::Mulh(node)
    }
}

impl NodeTrait for Mulh {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Mulh {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Mulh {}", self.node_id())
    }
}
impl fmt::Debug for Mulh {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn sel(self) -> Node {
        let unwrapped = unsafe { bindings::get_Mux_sel(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets value making the output selection.
    #[allow(clippy::let_and_return)]
    pub fn set_sel(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Mux_sel(self.0, unwrapped);
        }
    }

    /// Gets selected if sel input is false.
    #[allow(clippy::let_and_return)]
    pub fn false_(self) -> Node {
        let unwrapped = unsafe { bindings::get_Mux_false(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets selected if sel input is false.
    #[allow(clippy::let_and_return)]
    pub fn set_false_(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Mux_false(self.0, unwrapped);
        }
    }

    /// Gets selected if sel input is true.
    #[allow(clippy::let_and_return)]
    pub fn true_(self) -> Node {
        let unwrapped = unsafe { bindings::get_Mux_true(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets selected if sel input is true.
    #[allow(clippy::let_and_return)]
    pub fn set_true_(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Mux_true(self.0, unwrapped);
        }
    }
}

impl From<Mux> for Node {
    fn from(node: Mux) -> Node {
        Node::Mux(node)
    }
}

impl NodeTrait for Mux {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Mux {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Mux {}", self.node_id())
    }
}
impl fmt::Debug for Mux {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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

impl From<NoMem> for Node {
    fn from(node: NoMem) -> Node {
        Node::NoMem(node)
    }
}

impl NodeTrait for NoMem {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for NoMem {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "NoMem {}", self.node_id())
    }
}
impl fmt::Debug for NoMem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn op(self) -> Node {
        let unwrapped = unsafe { bindings::get_Not_op(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets operand.
    #[allow(clippy::let_and_return)]
    pub fn set_op(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Not_op(self.0, unwrapped);
        }
    }
}

impl From<Not> for Node {
    fn from(node: Not) -> Node {
        Node::Not(node)
    }
}

impl NodeTrait for Not {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Not {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Not {}", self.node_id())
    }
}
impl fmt::Debug for Not {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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

impl From<Offset> for Node {
    fn from(node: Offset) -> Node {
        Node::Offset(node)
    }
}

impl NodeTrait for Offset {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Offset {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Offset {}", self.node_id())
    }
}
impl fmt::Debug for Offset {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn left(self) -> Node {
        let unwrapped = unsafe { bindings::get_Or_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    #[allow(clippy::let_and_return)]
    pub fn set_left(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Or_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    #[allow(clippy::let_and_return)]
    pub fn right(self) -> Node {
        let unwrapped = unsafe { bindings::get_Or_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    #[allow(clippy::let_and_return)]
    pub fn set_right(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Or_right(self.0, unwrapped);
        }
    }
}

impl From<Or> for Node {
    fn from(node: Or) -> Node {
        Node::Or(node)
    }
}

impl NodeTrait for Or {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Or {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Or {}", self.node_id())
    }
}
impl fmt::Debug for Or {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn loop_(self) -> i32 {
        let unwrapped = unsafe { bindings::get_Phi_loop(self.0) };
        unwrapped
    }

    /// Sets whether Phi represents the observable effect of a (possibly)
    /// nonterminating loop.
    #[allow(clippy::let_and_return)]
    pub fn set_loop_(self, val: i32) {
        let unwrapped = val;
        unsafe {
            bindings::set_Phi_loop(self.0, unwrapped);
        }
    }
}

impl From<Phi> for Node {
    fn from(node: Phi) -> Node {
        Node::Phi(node)
    }
}

impl NodeTrait for Phi {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Phi {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Phi {}", self.node_id())
    }
}
impl fmt::Debug for Phi {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn op(self) -> Node {
        let unwrapped = unsafe { bindings::get_Pin_op(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets value which is pinned.
    #[allow(clippy::let_and_return)]
    pub fn set_op(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Pin_op(self.0, unwrapped);
        }
    }
}

impl From<Pin> for Node {
    fn from(node: Pin) -> Node {
        Node::Pin(node)
    }
}

impl NodeTrait for Pin {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Pin {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Pin {}", self.node_id())
    }
}
impl fmt::Debug for Pin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn pred(self) -> Node {
        let unwrapped = unsafe { bindings::get_Proj_pred(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets the tuple value from which a part is extracted.
    #[allow(clippy::let_and_return)]
    pub fn set_pred(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Proj_pred(self.0, unwrapped);
        }
    }

    /// Gets number of tuple component to be extracted.
    #[allow(clippy::let_and_return)]
    pub fn num(self) -> ::std::os::raw::c_uint {
        let unwrapped = unsafe { bindings::get_Proj_num(self.0) };
        unwrapped
    }

    /// Sets number of tuple component to be extracted.
    #[allow(clippy::let_and_return)]
    pub fn set_num(self, val: ::std::os::raw::c_uint) {
        let unwrapped = val;
        unsafe {
            bindings::set_Proj_num(self.0, unwrapped);
        }
    }
}

impl From<Proj> for Node {
    fn from(node: Proj) -> Node {
        Node::Proj(node, node.kind())
    }
}

impl NodeTrait for Proj {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl fmt::Debug for Proj {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn mem(self) -> Node {
        let unwrapped = unsafe { bindings::get_Raise_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    #[allow(clippy::let_and_return)]
    pub fn set_mem(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Raise_mem(self.0, unwrapped);
        }
    }

    /// Gets pointer to exception object to be thrown.
    #[allow(clippy::let_and_return)]
    pub fn exo_ptr(self) -> Node {
        let unwrapped = unsafe { bindings::get_Raise_exo_ptr(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets pointer to exception object to be thrown.
    #[allow(clippy::let_and_return)]
    pub fn set_exo_ptr(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Raise_exo_ptr(self.0, unwrapped);
        }
    }

    /// memory result.
    pub fn new_proj_m(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::M, 0) })
    }

    /// control flow to exception handler.
    pub fn new_proj_x(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 1) })
    }

    /// memory result.
    pub fn out_proj_m(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Raise_M(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow to exception handler.
    pub fn out_proj_x(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Raise_X(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }
}

impl From<Raise> for Node {
    fn from(node: Raise) -> Node {
        Node::Raise(node)
    }
}

impl NodeTrait for Raise {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Raise {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Raise {}", self.node_id())
    }
}
impl fmt::Debug for Raise {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn mem(self) -> Node {
        let unwrapped = unsafe { bindings::get_Return_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    #[allow(clippy::let_and_return)]
    pub fn set_mem(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Return_mem(self.0, unwrapped);
        }
    }
}

impl From<Return> for Node {
    fn from(node: Return) -> Node {
        Node::Return(node)
    }
}

impl NodeTrait for Return {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Return {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Return {}", self.node_id())
    }
}
impl fmt::Debug for Return {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn ptr(self) -> Node {
        let unwrapped = unsafe { bindings::get_Sel_ptr(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets pointer to array to select from.
    #[allow(clippy::let_and_return)]
    pub fn set_ptr(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Sel_ptr(self.0, unwrapped);
        }
    }

    /// Gets index to select.
    #[allow(clippy::let_and_return)]
    pub fn index(self) -> Node {
        let unwrapped = unsafe { bindings::get_Sel_index(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets index to select.
    #[allow(clippy::let_and_return)]
    pub fn set_index(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Sel_index(self.0, unwrapped);
        }
    }

    /// Gets array type.
    #[allow(clippy::let_and_return)]
    pub fn ty(self) -> Ty {
        let unwrapped = unsafe { bindings::get_Sel_type(self.0) };
        Ty::from_ir_type(unwrapped)
    }

    /// Sets array type.
    #[allow(clippy::let_and_return)]
    pub fn set_ty(self, val: Ty) {
        let unwrapped = val.ir_type();
        unsafe {
            bindings::set_Sel_type(self.0, unwrapped);
        }
    }
}

impl From<Sel> for Node {
    fn from(node: Sel) -> Node {
        Node::Sel(node)
    }
}

impl NodeTrait for Sel {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Sel {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Sel {}", self.node_id())
    }
}
impl fmt::Debug for Sel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn left(self) -> Node {
        let unwrapped = unsafe { bindings::get_Shl_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    #[allow(clippy::let_and_return)]
    pub fn set_left(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Shl_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    #[allow(clippy::let_and_return)]
    pub fn right(self) -> Node {
        let unwrapped = unsafe { bindings::get_Shl_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    #[allow(clippy::let_and_return)]
    pub fn set_right(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Shl_right(self.0, unwrapped);
        }
    }
}

impl From<Shl> for Node {
    fn from(node: Shl) -> Node {
        Node::Shl(node)
    }
}

impl NodeTrait for Shl {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Shl {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Shl {}", self.node_id())
    }
}
impl fmt::Debug for Shl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn left(self) -> Node {
        let unwrapped = unsafe { bindings::get_Shr_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    #[allow(clippy::let_and_return)]
    pub fn set_left(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Shr_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    #[allow(clippy::let_and_return)]
    pub fn right(self) -> Node {
        let unwrapped = unsafe { bindings::get_Shr_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    #[allow(clippy::let_and_return)]
    pub fn set_right(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Shr_right(self.0, unwrapped);
        }
    }
}

impl From<Shr> for Node {
    fn from(node: Shr) -> Node {
        Node::Shr(node)
    }
}

impl NodeTrait for Shr {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Shr {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Shr {}", self.node_id())
    }
}
impl fmt::Debug for Shr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn left(self) -> Node {
        let unwrapped = unsafe { bindings::get_Shrs_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    #[allow(clippy::let_and_return)]
    pub fn set_left(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Shrs_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    #[allow(clippy::let_and_return)]
    pub fn right(self) -> Node {
        let unwrapped = unsafe { bindings::get_Shrs_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    #[allow(clippy::let_and_return)]
    pub fn set_right(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Shrs_right(self.0, unwrapped);
        }
    }
}

impl From<Shrs> for Node {
    fn from(node: Shrs) -> Node {
        Node::Shrs(node)
    }
}

impl NodeTrait for Shrs {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Shrs {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Shrs {}", self.node_id())
    }
}
impl fmt::Debug for Shrs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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

impl From<Size> for Node {
    fn from(node: Size) -> Node {
        Node::Size(node)
    }
}

impl NodeTrait for Size {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Size {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Size {}", self.node_id())
    }
}
impl fmt::Debug for Size {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    pub fn new_proj_m(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::M, 0) })
    }

    /// frame base pointer.
    pub fn new_proj_p_frame_base(self, mode: Mode) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, mode.libfirm_mode(), 1) })
    }

    /// function arguments.
    pub fn new_proj_t_args(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::T, 2) })
    }

    /// initial memory.
    pub fn out_proj_m(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Start_M(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// frame base pointer.
    pub fn out_proj_p_frame_base(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Start_PFrameBase(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// function arguments.
    pub fn out_proj_t_args(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Start_TArgs(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }
}

impl From<Start> for Node {
    fn from(node: Start) -> Node {
        Node::Start(node)
    }
}

impl NodeTrait for Start {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Start {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Start {}", self.node_id())
    }
}
impl fmt::Debug for Start {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn mem(self) -> Node {
        let unwrapped = unsafe { bindings::get_Store_mem(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets memory dependency.
    #[allow(clippy::let_and_return)]
    pub fn set_mem(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Store_mem(self.0, unwrapped);
        }
    }

    /// Gets address to store to.
    #[allow(clippy::let_and_return)]
    pub fn ptr(self) -> Node {
        let unwrapped = unsafe { bindings::get_Store_ptr(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets address to store to.
    #[allow(clippy::let_and_return)]
    pub fn set_ptr(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Store_ptr(self.0, unwrapped);
        }
    }

    /// Gets value to store.
    #[allow(clippy::let_and_return)]
    pub fn value(self) -> Node {
        let unwrapped = unsafe { bindings::get_Store_value(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets value to store.
    #[allow(clippy::let_and_return)]
    pub fn set_value(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Store_value(self.0, unwrapped);
        }
    }

    /// Gets The type of the object which is stored at ptr (need not match with
    /// value's type).
    #[allow(clippy::let_and_return)]
    pub fn ty(self) -> Ty {
        let unwrapped = unsafe { bindings::get_Store_type(self.0) };
        Ty::from_ir_type(unwrapped)
    }

    /// Sets The type of the object which is stored at ptr (need not match with
    /// value's type).
    #[allow(clippy::let_and_return)]
    pub fn set_ty(self, val: Ty) {
        let unwrapped = val.ir_type();
        unsafe {
            bindings::set_Store_type(self.0, unwrapped);
        }
    }

    /// Gets volatile stores are a visible side-effect and may not be optimized.
    #[allow(clippy::let_and_return)]
    pub fn volatility(self) -> bindings::ir_volatility::Type {
        let unwrapped = unsafe { bindings::get_Store_volatility(self.0) };
        unwrapped
    }

    /// Sets volatile stores are a visible side-effect and may not be optimized.
    #[allow(clippy::let_and_return)]
    pub fn set_volatility(self, val: bindings::ir_volatility::Type) {
        let unwrapped = val;
        unsafe {
            bindings::set_Store_volatility(self.0, unwrapped);
        }
    }

    /// Gets pointers to unaligned stores don't need to respect the
    /// load-mode/type alignments.
    #[allow(clippy::let_and_return)]
    pub fn unaligned(self) -> bindings::ir_align::Type {
        let unwrapped = unsafe { bindings::get_Store_unaligned(self.0) };
        unwrapped
    }

    /// Sets pointers to unaligned stores don't need to respect the
    /// load-mode/type alignments.
    #[allow(clippy::let_and_return)]
    pub fn set_unaligned(self, val: bindings::ir_align::Type) {
        let unwrapped = val;
        unsafe {
            bindings::set_Store_unaligned(self.0, unwrapped);
        }
    }

    /// memory result.
    pub fn new_proj_m(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::M, 0) })
    }

    /// control flow when no exception occurs.
    pub fn new_proj_x_regular(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 1) })
    }

    /// control flow when exception occurred.
    pub fn new_proj_x_except(self) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, bindings::mode::X, 2) })
    }

    /// memory result.
    pub fn out_proj_m(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Store_M(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow when no exception occurs.
    pub fn out_proj_x_regular(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Store_XRegular(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }

    /// control flow when exception occurred.
    pub fn out_proj_x_except(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Store_XExcept(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }
}

impl From<Store> for Node {
    fn from(node: Store) -> Node {
        Node::Store(node)
    }
}

impl NodeTrait for Store {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl fmt::Debug for Store {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn left(self) -> Node {
        let unwrapped = unsafe { bindings::get_Sub_left(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets first operand.
    #[allow(clippy::let_and_return)]
    pub fn set_left(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Sub_left(self.0, unwrapped);
        }
    }

    /// Gets second operand.
    #[allow(clippy::let_and_return)]
    pub fn right(self) -> Node {
        let unwrapped = unsafe { bindings::get_Sub_right(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets second operand.
    #[allow(clippy::let_and_return)]
    pub fn set_right(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Sub_right(self.0, unwrapped);
        }
    }
}

impl From<Sub> for Node {
    fn from(node: Sub) -> Node {
        Node::Sub(node)
    }
}

impl NodeTrait for Sub {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Sub {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Sub {}", self.node_id())
    }
}
impl fmt::Debug for Sub {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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
    #[allow(clippy::let_and_return)]
    pub fn selector(self) -> Node {
        let unwrapped = unsafe { bindings::get_Switch_selector(self.0) };
        NodeFactory::node(unwrapped)
    }

    /// Sets input selector.
    #[allow(clippy::let_and_return)]
    pub fn set_selector(self, val: impl NodeTrait) {
        let unwrapped = val.internal_ir_node();
        unsafe {
            bindings::set_Switch_selector(self.0, unwrapped);
        }
    }

    /// Gets number of outputs (including pn_Switch_default).
    #[allow(clippy::let_and_return)]
    pub fn n_outs(self) -> ::std::os::raw::c_uint {
        let unwrapped = unsafe { bindings::get_Switch_n_outs(self.0) };
        unwrapped
    }

    /// Sets number of outputs (including pn_Switch_default).
    #[allow(clippy::let_and_return)]
    pub fn set_n_outs(self, val: ::std::os::raw::c_uint) {
        let unwrapped = val;
        unsafe {
            bindings::set_Switch_n_outs(self.0, unwrapped);
        }
    }

    /// Gets table describing mapping from input values to Proj numbers.
    #[allow(clippy::let_and_return)]
    pub fn table(self) -> *mut bindings::ir_switch_table {
        let unwrapped = unsafe { bindings::get_Switch_table(self.0) };
        unwrapped
    }

    /// Sets table describing mapping from input values to Proj numbers.
    #[allow(clippy::let_and_return)]
    pub fn set_table(self, val: *mut bindings::ir_switch_table) {
        let unwrapped = val;
        unsafe {
            bindings::set_Switch_table(self.0, unwrapped);
        }
    }

    /// control flow if no other case matches.
    pub fn new_proj_default(self, mode: Mode) -> Proj {
        Proj::new(unsafe { bindings::new_r_Proj(self.0, mode.libfirm_mode(), 0) })
    }

    /// control flow if no other case matches.
    pub fn out_proj_default(self) -> Option<Proj> {
        for out_node in self.out_nodes() {
            if let Node::Proj(proj, ProjKind::Switch_Default(_)) = out_node {
                return Some(proj);
            }
        }
        None
    }
}

impl From<Switch> for Node {
    fn from(node: Switch) -> Node {
        Node::Switch(node)
    }
}

impl NodeTrait for Switch {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Switch {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Switch {}", self.node_id())
    }
}
impl fmt::Debug for Switch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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

impl From<Sync> for Node {
    fn from(node: Sync) -> Node {
        Node::Sync(node)
    }
}

impl NodeTrait for Sync {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Sync {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Sync {}", self.node_id())
    }
}
impl fmt::Debug for Sync {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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

impl From<Tuple> for Node {
    fn from(node: Tuple) -> Node {
        Node::Tuple(node)
    }
}

impl NodeTrait for Tuple {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Tuple {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Tuple {}", self.node_id())
    }
}
impl fmt::Debug for Tuple {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
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

impl From<Unknown> for Node {
    fn from(node: Unknown) -> Node {
        Node::Unknown(node)
    }
}

impl NodeTrait for Unknown {
    fn internal_ir_node(&self) -> *mut bindings::ir_node {
        self.0
    }
}

impl NodeDebug for Unknown {
    fn fmt(&self, f: &mut fmt::Formatter, _opts: NodeDebugOpts) -> fmt::Result {
        write!(f, "Unknown {}", self.node_id())
    }
}
impl fmt::Debug for Unknown {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.debug_fmt(), f)
    }
}
impl Graph {
    /// Creates a new Add-node.
    /// * `block` The block.
    /// * `irn_left` left
    /// * `irn_right` right
    #[allow(clippy::style)]
    pub fn new_add(self, block: Block, irn_left: impl NodeTrait, irn_right: impl NodeTrait) -> Add {
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
    #[allow(clippy::style)]
    pub fn new_address(self, entity: Entity) -> Address {
        let ir_node = unsafe { bindings::new_r_Address(self.irg, entity.into()) };
        Address::new(ir_node)
    }

    /// Creates a new Align-node.
    /// * `mode` mode of the operations result
    /// * `ty` type to operate on
    #[allow(clippy::style)]
    pub fn new_align(self, mode: Mode, ty: Ty) -> Align {
        let ir_node = unsafe { bindings::new_r_Align(self.irg, mode.libfirm_mode(), ty.ir_type()) };
        Align::new(ir_node)
    }

    /// Creates a new Alloc-node.
    /// * `block` The block.
    /// * `irn_mem` mem
    /// * `irn_size` size
    /// * `alignment` alignment of the memory block (must be a power of 2)
    #[allow(clippy::style)]
    pub fn new_alloc(
        self,
        block: Block,
        irn_mem: impl NodeTrait,
        irn_size: impl NodeTrait,
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
    #[allow(clippy::style)]
    pub fn new_and(self, block: Block, irn_left: impl NodeTrait, irn_right: impl NodeTrait) -> And {
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
    #[allow(clippy::style)]
    pub fn new_bad(self, mode: Mode) -> Bad {
        let ir_node = unsafe { bindings::new_r_Bad(self.irg, mode.libfirm_mode()) };
        Bad::new(ir_node)
    }

    /// Creates a new Bitcast-node.
    /// * `block` The block.
    /// * `irn_op` op
    /// * `mode` mode of the operations result
    #[allow(clippy::style)]
    pub fn new_bitcast(self, block: Block, irn_op: impl NodeTrait, mode: Mode) -> Bitcast {
        let ir_node = unsafe {
            bindings::new_r_Bitcast(
                block.internal_ir_node(),
                irn_op.internal_ir_node(),
                mode.libfirm_mode(),
            )
        };
        Bitcast::new(ir_node)
    }

    /// Creates a new Block-node.
    /// * `in_` additional inputs
    #[allow(clippy::style)]
    pub fn new_block(self, in_: &[Node]) -> Block {
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
    #[allow(clippy::style)]
    pub fn new_builtin(
        self,
        block: Block,
        irn_mem: impl NodeTrait,
        in_: &[Node],
        kind: bindings::ir_builtin_kind::Type,
        ty: Ty,
    ) -> Builtin {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe {
            bindings::new_r_Builtin(
                block.internal_ir_node(),
                irn_mem.internal_ir_node(),
                in_.len() as i32,
                in_.as_ptr(),
                kind,
                ty.ir_type(),
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
    #[allow(clippy::style)]
    pub fn new_call(
        self,
        block: Block,
        irn_mem: impl NodeTrait,
        irn_ptr: impl NodeTrait,
        in_: &[Node],
        ty: Ty,
    ) -> Call {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe {
            bindings::new_r_Call(
                block.internal_ir_node(),
                irn_mem.internal_ir_node(),
                irn_ptr.internal_ir_node(),
                in_.len() as i32,
                in_.as_ptr(),
                ty.ir_type(),
            )
        };
        Call::new(ir_node)
    }

    /// Creates a new Cmp-node.
    /// * `block` The block.
    /// * `irn_left` left
    /// * `irn_right` right
    /// * `relation` Comparison relation
    #[allow(clippy::style)]
    pub fn new_cmp(
        self,
        block: Block,
        irn_left: impl NodeTrait,
        irn_right: impl NodeTrait,
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
    #[allow(clippy::style)]
    pub fn new_cond(self, block: Block, irn_selector: impl NodeTrait) -> Cond {
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
    #[allow(clippy::style)]
    pub fn new_confirm(
        self,
        block: Block,
        irn_value: impl NodeTrait,
        irn_bound: impl NodeTrait,
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
    #[allow(clippy::style)]
    pub fn new_const(self, tarval: Tarval) -> Const {
        let ir_node = unsafe { bindings::new_r_Const(self.irg, tarval.into()) };
        Const::new(ir_node)
    }

    /// Creates a new Conv-node.
    /// * `block` The block.
    /// * `irn_op` op
    /// * `mode` mode of the operations result
    #[allow(clippy::style)]
    pub fn new_conv(self, block: Block, irn_op: impl NodeTrait, mode: Mode) -> Conv {
        let ir_node = unsafe {
            bindings::new_r_Conv(
                block.internal_ir_node(),
                irn_op.internal_ir_node(),
                mode.libfirm_mode(),
            )
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
    #[allow(clippy::style)]
    pub fn new_copyb(
        self,
        block: Block,
        irn_mem: impl NodeTrait,
        irn_dst: impl NodeTrait,
        irn_src: impl NodeTrait,
        ty: Ty,
        flags: bindings::ir_cons_flags::Type,
    ) -> CopyB {
        let ir_node = unsafe {
            bindings::new_r_CopyB(
                block.internal_ir_node(),
                irn_mem.internal_ir_node(),
                irn_dst.internal_ir_node(),
                irn_src.internal_ir_node(),
                ty.ir_type(),
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
    #[allow(clippy::style)]
    pub fn new_div(
        self,
        block: Block,
        irn_mem: impl NodeTrait,
        irn_left: impl NodeTrait,
        irn_right: impl NodeTrait,
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
    #[allow(clippy::style)]
    pub fn new_dummy(self, mode: Mode) -> Dummy {
        let ir_node = unsafe { bindings::new_r_Dummy(self.irg, mode.libfirm_mode()) };
        Dummy::new(ir_node)
    }

    /// Creates a new End-node.
    /// * `in_` additional inputs
    #[allow(clippy::style)]
    pub fn new_end(self, in_: &[Node]) -> End {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe { bindings::new_r_End(self.irg, in_.len() as i32, in_.as_ptr()) };
        End::new(ir_node)
    }

    /// Creates a new Eor-node.
    /// * `block` The block.
    /// * `irn_left` left
    /// * `irn_right` right
    #[allow(clippy::style)]
    pub fn new_eor(self, block: Block, irn_left: impl NodeTrait, irn_right: impl NodeTrait) -> Eor {
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
    #[allow(clippy::style)]
    pub fn new_free(self, block: Block, irn_mem: impl NodeTrait, irn_ptr: impl NodeTrait) -> Free {
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
    #[allow(clippy::style)]
    pub fn new_ijmp(self, block: Block, irn_target: impl NodeTrait) -> IJmp {
        let ir_node = unsafe {
            bindings::new_r_IJmp(block.internal_ir_node(), irn_target.internal_ir_node())
        };
        IJmp::new(ir_node)
    }

    /// Creates a new Jmp-node.
    /// * `block` The block.
    #[allow(clippy::style)]
    pub fn new_jmp(self, block: Block) -> Jmp {
        let ir_node = unsafe { bindings::new_r_Jmp(block.internal_ir_node()) };
        Jmp::new(ir_node)
    }

    /// Creates a new Load-node.
    /// * `block` The block.
    /// * `irn_mem` mem
    /// * `irn_ptr` ptr
    /// * `mode` mode of the value to be loaded
    /// * `ty` The type of the object which is stored at ptr (need not match
    ///   with mode)
    /// * `flags` specifies alignment, volatility and pin state
    #[allow(clippy::style)]
    pub fn new_load(
        self,
        block: Block,
        irn_mem: impl NodeTrait,
        irn_ptr: impl NodeTrait,
        mode: Mode,
        ty: Ty,
        flags: bindings::ir_cons_flags::Type,
    ) -> Load {
        let ir_node = unsafe {
            bindings::new_r_Load(
                block.internal_ir_node(),
                irn_mem.internal_ir_node(),
                irn_ptr.internal_ir_node(),
                mode.libfirm_mode(),
                ty.ir_type(),
                flags,
            )
        };
        Load::new(ir_node)
    }

    /// Creates a new Member-node.
    /// * `block` The block.
    /// * `irn_ptr` ptr
    /// * `entity` entity which is selected
    #[allow(clippy::style)]
    pub fn new_member(self, block: Block, irn_ptr: impl NodeTrait, entity: Entity) -> Member {
        let ir_node = unsafe {
            bindings::new_r_Member(
                block.internal_ir_node(),
                irn_ptr.internal_ir_node(),
                entity.into(),
            )
        };
        Member::new(ir_node)
    }

    /// Creates a new Minus-node.
    /// * `block` The block.
    /// * `irn_op` op
    #[allow(clippy::style)]
    pub fn new_minus(self, block: Block, irn_op: impl NodeTrait) -> Minus {
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
    #[allow(clippy::style)]
    pub fn new_mod(
        self,
        block: Block,
        irn_mem: impl NodeTrait,
        irn_left: impl NodeTrait,
        irn_right: impl NodeTrait,
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
    #[allow(clippy::style)]
    pub fn new_mul(self, block: Block, irn_left: impl NodeTrait, irn_right: impl NodeTrait) -> Mul {
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
    #[allow(clippy::style)]
    pub fn new_mulh(
        self,
        block: Block,
        irn_left: impl NodeTrait,
        irn_right: impl NodeTrait,
    ) -> Mulh {
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
    #[allow(clippy::style)]
    pub fn new_mux(
        self,
        block: Block,
        irn_sel: impl NodeTrait,
        irn_false: impl NodeTrait,
        irn_true: impl NodeTrait,
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
    #[allow(clippy::style)]
    pub fn new_nomem(self) -> NoMem {
        let ir_node = unsafe { bindings::new_r_NoMem(self.irg) };
        NoMem::new(ir_node)
    }

    /// Creates a new Not-node.
    /// * `block` The block.
    /// * `irn_op` op
    #[allow(clippy::style)]
    pub fn new_not(self, block: Block, irn_op: impl NodeTrait) -> Not {
        let ir_node =
            unsafe { bindings::new_r_Not(block.internal_ir_node(), irn_op.internal_ir_node()) };
        Not::new(ir_node)
    }

    /// Creates a new Offset-node.
    /// * `mode` mode of the operations result
    /// * `entity` entity to operate on
    #[allow(clippy::style)]
    pub fn new_offset(self, mode: Mode, entity: Entity) -> Offset {
        let ir_node =
            unsafe { bindings::new_r_Offset(self.irg, mode.libfirm_mode(), entity.into()) };
        Offset::new(ir_node)
    }

    /// Creates a new Or-node.
    /// * `block` The block.
    /// * `irn_left` left
    /// * `irn_right` right
    #[allow(clippy::style)]
    pub fn new_or(self, block: Block, irn_left: impl NodeTrait, irn_right: impl NodeTrait) -> Or {
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
    #[allow(clippy::style)]
    pub fn new_phi(self, block: Block, in_: &[Node], mode: Mode) -> Phi {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe {
            bindings::new_r_Phi(
                block.internal_ir_node(),
                in_.len() as i32,
                in_.as_ptr(),
                mode.libfirm_mode(),
            )
        };
        Phi::new(ir_node)
    }

    /// Creates a new Pin-node.
    /// * `block` The block.
    /// * `irn_op` op
    #[allow(clippy::style)]
    pub fn new_pin(self, block: Block, irn_op: impl NodeTrait) -> Pin {
        let ir_node =
            unsafe { bindings::new_r_Pin(block.internal_ir_node(), irn_op.internal_ir_node()) };
        Pin::new(ir_node)
    }

    /// Creates a new Proj-node.
    /// * `irn_pred` pred
    /// * `mode` mode of the operations result
    /// * `num` number of tuple component to be extracted
    #[allow(clippy::style)]
    pub fn new_proj(
        self,
        irn_pred: impl NodeTrait,
        mode: Mode,
        num: ::std::os::raw::c_uint,
    ) -> Proj {
        let ir_node =
            unsafe { bindings::new_r_Proj(irn_pred.internal_ir_node(), mode.libfirm_mode(), num) };
        Proj::new(ir_node)
    }

    /// Creates a new Raise-node.
    /// * `block` The block.
    /// * `irn_mem` mem
    /// * `irn_exo_ptr` exo_ptr
    #[allow(clippy::style)]
    pub fn new_raise(
        self,
        block: Block,
        irn_mem: impl NodeTrait,
        irn_exo_ptr: impl NodeTrait,
    ) -> Raise {
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
    #[allow(clippy::style)]
    pub fn new_return(self, block: Block, irn_mem: impl NodeTrait, in_: &[Node]) -> Return {
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
    #[allow(clippy::style)]
    pub fn new_sel(
        self,
        block: Block,
        irn_ptr: impl NodeTrait,
        irn_index: impl NodeTrait,
        ty: Ty,
    ) -> Sel {
        let ir_node = unsafe {
            bindings::new_r_Sel(
                block.internal_ir_node(),
                irn_ptr.internal_ir_node(),
                irn_index.internal_ir_node(),
                ty.ir_type(),
            )
        };
        Sel::new(ir_node)
    }

    /// Creates a new Shl-node.
    /// * `block` The block.
    /// * `irn_left` left
    /// * `irn_right` right
    #[allow(clippy::style)]
    pub fn new_shl(self, block: Block, irn_left: impl NodeTrait, irn_right: impl NodeTrait) -> Shl {
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
    #[allow(clippy::style)]
    pub fn new_shr(self, block: Block, irn_left: impl NodeTrait, irn_right: impl NodeTrait) -> Shr {
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
    #[allow(clippy::style)]
    pub fn new_shrs(
        self,
        block: Block,
        irn_left: impl NodeTrait,
        irn_right: impl NodeTrait,
    ) -> Shrs {
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
    #[allow(clippy::style)]
    pub fn new_size(self, mode: Mode, ty: Ty) -> Size {
        let ir_node = unsafe { bindings::new_r_Size(self.irg, mode.libfirm_mode(), ty.ir_type()) };
        Size::new(ir_node)
    }

    /// Creates a new Start-node.
    #[allow(clippy::style)]
    pub fn new_start(self) -> Start {
        let ir_node = unsafe { bindings::new_r_Start(self.irg) };
        Start::new(ir_node)
    }

    /// Creates a new Store-node.
    /// * `block` The block.
    /// * `irn_mem` mem
    /// * `irn_ptr` ptr
    /// * `irn_value` value
    /// * `ty` The type of the object which is stored at ptr (need not match
    ///   with value's type)
    /// * `flags` specifies alignment, volatility and pin state
    #[allow(clippy::style)]
    pub fn new_store(
        self,
        block: Block,
        irn_mem: impl NodeTrait,
        irn_ptr: impl NodeTrait,
        irn_value: impl NodeTrait,
        ty: Ty,
        flags: bindings::ir_cons_flags::Type,
    ) -> Store {
        let ir_node = unsafe {
            bindings::new_r_Store(
                block.internal_ir_node(),
                irn_mem.internal_ir_node(),
                irn_ptr.internal_ir_node(),
                irn_value.internal_ir_node(),
                ty.ir_type(),
                flags,
            )
        };
        Store::new(ir_node)
    }

    /// Creates a new Sub-node.
    /// * `block` The block.
    /// * `irn_left` left
    /// * `irn_right` right
    #[allow(clippy::style)]
    pub fn new_sub(self, block: Block, irn_left: impl NodeTrait, irn_right: impl NodeTrait) -> Sub {
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
    #[allow(clippy::style)]
    pub fn new_switch(
        self,
        block: Block,
        irn_selector: impl NodeTrait,
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
    #[allow(clippy::style)]
    pub fn new_sync(self, block: Block, in_: &[Node]) -> Sync {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe {
            bindings::new_r_Sync(block.internal_ir_node(), in_.len() as i32, in_.as_ptr())
        };
        Sync::new(ir_node)
    }

    /// Creates a new Tuple-node.
    /// * `block` The block.
    /// * `in_` additional inputs
    #[allow(clippy::style)]
    pub fn new_tuple(self, block: Block, in_: &[Node]) -> Tuple {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe {
            bindings::new_r_Tuple(block.internal_ir_node(), in_.len() as i32, in_.as_ptr())
        };
        Tuple::new(ir_node)
    }

    /// Creates a new Unknown-node.
    /// * `mode` mode of the operations result
    #[allow(clippy::style)]
    pub fn new_unknown(self, mode: Mode) -> Unknown {
        let ir_node = unsafe { bindings::new_r_Unknown(self.irg, mode.libfirm_mode()) };
        Unknown::new(ir_node)
    }
}

impl Block {
    /// Creates a new Add-node.
    /// * `irn_left` left
    /// * `irn_right` right
    #[allow(clippy::style)]
    pub fn new_add(self, irn_left: impl NodeTrait, irn_right: impl NodeTrait) -> Add {
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
    #[allow(clippy::style)]
    pub fn new_alloc(
        self,
        irn_mem: impl NodeTrait,
        irn_size: impl NodeTrait,
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
    #[allow(clippy::style)]
    pub fn new_and(self, irn_left: impl NodeTrait, irn_right: impl NodeTrait) -> And {
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
    #[allow(clippy::style)]
    pub fn new_bitcast(self, irn_op: impl NodeTrait, mode: Mode) -> Bitcast {
        let ir_node = unsafe {
            bindings::new_r_Bitcast(self.0, irn_op.internal_ir_node(), mode.libfirm_mode())
        };
        Bitcast::new(ir_node)
    }

    /// Creates a new Builtin-node.
    /// * `irn_mem` mem
    /// * `in_` additional inputs
    /// * `kind` kind of builtin
    /// * `ty` method type for the builtin call
    #[allow(clippy::style)]
    pub fn new_builtin(
        self,
        irn_mem: impl NodeTrait,
        in_: &[Node],
        kind: bindings::ir_builtin_kind::Type,
        ty: Ty,
    ) -> Builtin {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe {
            bindings::new_r_Builtin(
                self.0,
                irn_mem.internal_ir_node(),
                in_.len() as i32,
                in_.as_ptr(),
                kind,
                ty.ir_type(),
            )
        };
        Builtin::new(ir_node)
    }

    /// Creates a new Call-node.
    /// * `irn_mem` mem
    /// * `irn_ptr` ptr
    /// * `in_` additional inputs
    /// * `ty` type of the call (usually type of the called procedure)
    #[allow(clippy::style)]
    pub fn new_call(
        self,
        irn_mem: impl NodeTrait,
        irn_ptr: impl NodeTrait,
        in_: &[Node],
        ty: Ty,
    ) -> Call {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe {
            bindings::new_r_Call(
                self.0,
                irn_mem.internal_ir_node(),
                irn_ptr.internal_ir_node(),
                in_.len() as i32,
                in_.as_ptr(),
                ty.ir_type(),
            )
        };
        Call::new(ir_node)
    }

    /// Creates a new Cmp-node.
    /// * `irn_left` left
    /// * `irn_right` right
    /// * `relation` Comparison relation
    #[allow(clippy::style)]
    pub fn new_cmp(
        self,
        irn_left: impl NodeTrait,
        irn_right: impl NodeTrait,
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
    #[allow(clippy::style)]
    pub fn new_cond(self, irn_selector: impl NodeTrait) -> Cond {
        let ir_node = unsafe { bindings::new_r_Cond(self.0, irn_selector.internal_ir_node()) };
        Cond::new(ir_node)
    }

    /// Creates a new Confirm-node.
    /// * `irn_value` value
    /// * `irn_bound` bound
    /// * `relation` relation of value to bound
    #[allow(clippy::style)]
    pub fn new_confirm(
        self,
        irn_value: impl NodeTrait,
        irn_bound: impl NodeTrait,
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
    #[allow(clippy::style)]
    pub fn new_conv(self, irn_op: impl NodeTrait, mode: Mode) -> Conv {
        let ir_node =
            unsafe { bindings::new_r_Conv(self.0, irn_op.internal_ir_node(), mode.libfirm_mode()) };
        Conv::new(ir_node)
    }

    /// Creates a new CopyB-node.
    /// * `irn_mem` mem
    /// * `irn_dst` dst
    /// * `irn_src` src
    /// * `ty` type of copied data
    /// * `flags` specifies volatility
    #[allow(clippy::style)]
    pub fn new_copyb(
        self,
        irn_mem: impl NodeTrait,
        irn_dst: impl NodeTrait,
        irn_src: impl NodeTrait,
        ty: Ty,
        flags: bindings::ir_cons_flags::Type,
    ) -> CopyB {
        let ir_node = unsafe {
            bindings::new_r_CopyB(
                self.0,
                irn_mem.internal_ir_node(),
                irn_dst.internal_ir_node(),
                irn_src.internal_ir_node(),
                ty.ir_type(),
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
    #[allow(clippy::style)]
    pub fn new_div(
        self,
        irn_mem: impl NodeTrait,
        irn_left: impl NodeTrait,
        irn_right: impl NodeTrait,
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
    #[allow(clippy::style)]
    pub fn new_eor(self, irn_left: impl NodeTrait, irn_right: impl NodeTrait) -> Eor {
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
    #[allow(clippy::style)]
    pub fn new_free(self, irn_mem: impl NodeTrait, irn_ptr: impl NodeTrait) -> Free {
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
    #[allow(clippy::style)]
    pub fn new_ijmp(self, irn_target: impl NodeTrait) -> IJmp {
        let ir_node = unsafe { bindings::new_r_IJmp(self.0, irn_target.internal_ir_node()) };
        IJmp::new(ir_node)
    }

    /// Creates a new Jmp-node.
    #[allow(clippy::style)]
    pub fn new_jmp(self) -> Jmp {
        let ir_node = unsafe { bindings::new_r_Jmp(self.0) };
        Jmp::new(ir_node)
    }

    /// Creates a new Load-node.
    /// * `irn_mem` mem
    /// * `irn_ptr` ptr
    /// * `mode` mode of the value to be loaded
    /// * `ty` The type of the object which is stored at ptr (need not match
    ///   with mode)
    /// * `flags` specifies alignment, volatility and pin state
    #[allow(clippy::style)]
    pub fn new_load(
        self,
        irn_mem: impl NodeTrait,
        irn_ptr: impl NodeTrait,
        mode: Mode,
        ty: Ty,
        flags: bindings::ir_cons_flags::Type,
    ) -> Load {
        let ir_node = unsafe {
            bindings::new_r_Load(
                self.0,
                irn_mem.internal_ir_node(),
                irn_ptr.internal_ir_node(),
                mode.libfirm_mode(),
                ty.ir_type(),
                flags,
            )
        };
        Load::new(ir_node)
    }

    /// Creates a new Member-node.
    /// * `irn_ptr` ptr
    /// * `entity` entity which is selected
    #[allow(clippy::style)]
    pub fn new_member(self, irn_ptr: impl NodeTrait, entity: Entity) -> Member {
        let ir_node =
            unsafe { bindings::new_r_Member(self.0, irn_ptr.internal_ir_node(), entity.into()) };
        Member::new(ir_node)
    }

    /// Creates a new Minus-node.
    /// * `irn_op` op
    #[allow(clippy::style)]
    pub fn new_minus(self, irn_op: impl NodeTrait) -> Minus {
        let ir_node = unsafe { bindings::new_r_Minus(self.0, irn_op.internal_ir_node()) };
        Minus::new(ir_node)
    }

    /// Creates a new Mod-node.
    /// * `irn_mem` mem
    /// * `irn_left` left
    /// * `irn_right` right
    /// * `pinned` pinned state
    #[allow(clippy::style)]
    pub fn new_mod(
        self,
        irn_mem: impl NodeTrait,
        irn_left: impl NodeTrait,
        irn_right: impl NodeTrait,
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
    #[allow(clippy::style)]
    pub fn new_mul(self, irn_left: impl NodeTrait, irn_right: impl NodeTrait) -> Mul {
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
    #[allow(clippy::style)]
    pub fn new_mulh(self, irn_left: impl NodeTrait, irn_right: impl NodeTrait) -> Mulh {
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
    #[allow(clippy::style)]
    pub fn new_mux(
        self,
        irn_sel: impl NodeTrait,
        irn_false: impl NodeTrait,
        irn_true: impl NodeTrait,
    ) -> Mux {
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
    #[allow(clippy::style)]
    pub fn new_not(self, irn_op: impl NodeTrait) -> Not {
        let ir_node = unsafe { bindings::new_r_Not(self.0, irn_op.internal_ir_node()) };
        Not::new(ir_node)
    }

    /// Creates a new Or-node.
    /// * `irn_left` left
    /// * `irn_right` right
    #[allow(clippy::style)]
    pub fn new_or(self, irn_left: impl NodeTrait, irn_right: impl NodeTrait) -> Or {
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
    #[allow(clippy::style)]
    pub fn new_phi(self, in_: &[Node], mode: Mode) -> Phi {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe {
            bindings::new_r_Phi(self.0, in_.len() as i32, in_.as_ptr(), mode.libfirm_mode())
        };
        Phi::new(ir_node)
    }

    /// Creates a new Pin-node.
    /// * `irn_op` op
    #[allow(clippy::style)]
    pub fn new_pin(self, irn_op: impl NodeTrait) -> Pin {
        let ir_node = unsafe { bindings::new_r_Pin(self.0, irn_op.internal_ir_node()) };
        Pin::new(ir_node)
    }

    /// Creates a new Raise-node.
    /// * `irn_mem` mem
    /// * `irn_exo_ptr` exo_ptr
    #[allow(clippy::style)]
    pub fn new_raise(self, irn_mem: impl NodeTrait, irn_exo_ptr: impl NodeTrait) -> Raise {
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
    #[allow(clippy::style)]
    pub fn new_return(self, irn_mem: impl NodeTrait, in_: &[Node]) -> Return {
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
    #[allow(clippy::style)]
    pub fn new_sel(self, irn_ptr: impl NodeTrait, irn_index: impl NodeTrait, ty: Ty) -> Sel {
        let ir_node = unsafe {
            bindings::new_r_Sel(
                self.0,
                irn_ptr.internal_ir_node(),
                irn_index.internal_ir_node(),
                ty.ir_type(),
            )
        };
        Sel::new(ir_node)
    }

    /// Creates a new Shl-node.
    /// * `irn_left` left
    /// * `irn_right` right
    #[allow(clippy::style)]
    pub fn new_shl(self, irn_left: impl NodeTrait, irn_right: impl NodeTrait) -> Shl {
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
    #[allow(clippy::style)]
    pub fn new_shr(self, irn_left: impl NodeTrait, irn_right: impl NodeTrait) -> Shr {
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
    #[allow(clippy::style)]
    pub fn new_shrs(self, irn_left: impl NodeTrait, irn_right: impl NodeTrait) -> Shrs {
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
    ///   with value's type)
    /// * `flags` specifies alignment, volatility and pin state
    #[allow(clippy::style)]
    pub fn new_store(
        self,
        irn_mem: impl NodeTrait,
        irn_ptr: impl NodeTrait,
        irn_value: impl NodeTrait,
        ty: Ty,
        flags: bindings::ir_cons_flags::Type,
    ) -> Store {
        let ir_node = unsafe {
            bindings::new_r_Store(
                self.0,
                irn_mem.internal_ir_node(),
                irn_ptr.internal_ir_node(),
                irn_value.internal_ir_node(),
                ty.ir_type(),
                flags,
            )
        };
        Store::new(ir_node)
    }

    /// Creates a new Sub-node.
    /// * `irn_left` left
    /// * `irn_right` right
    #[allow(clippy::style)]
    pub fn new_sub(self, irn_left: impl NodeTrait, irn_right: impl NodeTrait) -> Sub {
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
    #[allow(clippy::style)]
    pub fn new_switch(
        self,
        irn_selector: impl NodeTrait,
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
    #[allow(clippy::style)]
    pub fn new_sync(self, in_: &[Node]) -> Sync {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe { bindings::new_r_Sync(self.0, in_.len() as i32, in_.as_ptr()) };
        Sync::new(ir_node)
    }

    /// Creates a new Tuple-node.
    /// * `in_` additional inputs
    #[allow(clippy::style)]
    pub fn new_tuple(self, in_: &[Node]) -> Tuple {
        let in_: Vec<*mut bindings::ir_node> = in_.iter().map(|v| v.internal_ir_node()).collect();
        let ir_node = unsafe { bindings::new_r_Tuple(self.0, in_.len() as i32, in_.as_ptr()) };
        Tuple::new(ir_node)
    }
}
