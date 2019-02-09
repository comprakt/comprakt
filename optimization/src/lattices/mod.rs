mod heap;
mod mem;
pub use self::{heap::*, mem::*};
use libfirm_rs::{
    nodes::{Node, NodeDebug, NodeTrait, Phi},
    Mode, Tarval, TarvalKind,
};
use std::{fmt, rc::Rc};

pub enum JoinContext {
    None,
    Phi {
        phi: Phi,
        is_self_initial: bool,
        other_pred_idx: usize,
    },
}

pub trait Lattice: Eq + Clone {
    fn is_progression_of(&self, other: &Self) -> bool;
    fn join(&self, other: &Self, context: &JoinContext) -> Self;

    fn join_default(&self, other: &Self) -> Self {
        self.join(other, &JoinContext::None)
    }

    /*fn join_many<'t, I>(vals: I) -> Option<Self>
    where
        I: IntoIterator<Item = &'t Self>,
        Self: 't,
    {
        let mut cur: Option<Self> = None;
        for val in vals {
            cur = Some(if let Some(cur) = cur {
                cur.join(val)
            } else {
                val.clone()
            })
        }
        cur
    }*/
}

// == NodeLattice ==

#[derive(Clone, PartialEq, Eq)]
pub enum NodeLattice {
    NotReachableYet,
    Value(NodeValue),
    Heap(Rc<Heap>),
    Tuple(Box<NodeLattice>, Box<NodeLattice>),
    Invalid,
}

impl NodeLattice {
    pub fn start() -> NodeLattice {
        NodeLattice::NotReachableYet
    }

    pub fn reachable(&self) -> bool {
        match self {
            NodeLattice::NotReachableYet => false,
            _ => true,
        }
    }

    pub fn tuple(val1: NodeLattice, val2: NodeLattice) -> NodeLattice {
        NodeLattice::Tuple(Box::new(val1), Box::new(val2))
    }

    pub fn tuple_1(&self) -> &NodeLattice {
        match self {
            NodeLattice::Tuple(t1, _t2) => &t1,
            NodeLattice::NotReachableYet => &NodeLattice::NotReachableYet,
            val => panic!("Invalid data type {:?}", val),
        }
    }

    pub fn tuple_2(&self) -> &NodeLattice {
        match self {
            NodeLattice::Tuple(_t1, t2) => &t2,
            NodeLattice::NotReachableYet => &NodeLattice::NotReachableYet,
            val => panic!("Invalid data type {:?}", val),
        }
    }

    pub fn expect_value_or_no_info(&self) -> Option<&NodeValue> {
        match self {
            NodeLattice::Value(val) => Some(val),
            NodeLattice::NotReachableYet => None,
            _ => panic!("Expected NodeValue, but got: {:?}", self),
        }
    }

    fn from_tarval_optional_node(val: Tarval, mode: Mode, source: Option<Node>) -> Self {
        match val.kind() {
            TarvalKind::Unknown => NodeLattice::NotReachableYet,
            _ => NodeValue::from_known_tarval(val, mode, source).into(),
        }
    }

    pub fn from_tarval(val: Tarval, mode: Mode) -> Self {
        Self::from_tarval_optional_node(val, mode, None)
    }

    pub fn from_tarval_node(val: Tarval, source: Node) -> Self {
        Self::from_tarval_optional_node(val, source.mode(), Some(source))
    }
}

impl Lattice for NodeLattice {
    fn is_progression_of(&self, other: &Self) -> bool {
        use self::NodeLattice::*;
        match (self, other) {
            (_, NotReachableYet) => true,
            (Value(v1), Value(v2)) => v1.is_progression_of(v2),
            (Heap(heap1), Heap(heap2)) => heap1.is_progression_of(heap2),
            (Tuple(a1, a2), Tuple(b1, b2)) => a1.is_progression_of(b1) && a2.is_progression_of(b2),
            _ => false,
        }
    }

    fn join(&self, other: &Self, context: &JoinContext) -> Self {
        use self::NodeLattice::*;
        match (self, other) {
            (NotReachableYet, arg2) => arg2.clone(),
            (arg1, NotReachableYet) => arg1.clone(),
            (Value(val1), Value(val2)) => Value(val1.join(val2, context)),
            (Heap(heap1), Heap(heap2)) => Heap(Rc::new(heap1.join(heap2, context))),
            (Tuple(a1, a2), Tuple(b1, b2)) => {
                NodeLattice::tuple(a1.join(b1, context), a2.join(b2, context))
            }
            (val1, val2) => panic!(
                "Cannot join value {:?} with {:?} - they have different types.",
                val1, val2
            ),
        }
    }
}

impl fmt::Debug for NodeLattice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeLattice::NotReachableYet => write!(f, "🛇"),
            NodeLattice::Value(val) => write!(f, "{:?}", val),
            NodeLattice::Heap(heap) => write!(f, "{:?}", heap),
            NodeLattice::Tuple(..) => write!(f, "Tuple"),
            NodeLattice::Invalid => write!(f, "Invalid"),
        }
    }
}

impl From<NodeValue> for NodeLattice {
    fn from(val: NodeValue) -> Self {
        NodeLattice::Value(val)
    }
}

// == AbstractValue ==

#[derive(Eq, PartialEq, Clone)]
pub enum AbstractValue {
    Tarval(Tarval),
    Pointer(Pointer),
}

impl From<Pointer> for AbstractValue {
    fn from(pointer: Pointer) -> Self {
        AbstractValue::Pointer(pointer)
    }
}

impl From<Tarval> for AbstractValue {
    fn from(val: Tarval) -> Self {
        AbstractValue::Tarval(val)
    }
}

impl fmt::Debug for AbstractValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AbstractValue::Pointer(ptrs) => write!(f, "{:?}", ptrs),
            AbstractValue::Tarval(val) => write!(f, "{:?}", val),
        }
    }
}

impl Lattice for AbstractValue {
    fn is_progression_of(&self, _other: &Self) -> bool {
        // todo
        /*
                (
            NodeValue(self::Value::Tarval(val1), n1),
            NodeValue(self::Value::Tarval(val2), n2),
        ) => val1.lattice_eq(*val2) && is_option_progression_of(n1, n2),
        (NodeValue(self::Value::Pointer(p1), n1), NodeValue(self::Value::Pointer(p2), n2)) => {
            p1.is_progression_of(p2) && is_option_progression_of(n1, n2)
        }
        */
        true
    }

    fn join(&self, other: &Self, context: &JoinContext) -> Self {
        use self::AbstractValue::*;
        match (self, other) {
            (Tarval(val1), Tarval(val2)) => Tarval(val1.join(*val2)),
            (Pointer(ps1), Pointer(ps2)) => Pointer(ps1.join(ps2, context)),
            /*(Pointer(ps), Tarval(tval)) | (Tarval(tval), Pointer(ps)) if ps.is_null() => {
                uncomment if panic below happens
                log::debug!(
                    "Join {:?} with {:?} that should happen\
                     only when load store is disabled",
                    ps,
                    tval
                );
                Tarval(*tval)
            }*/
            (val1, val2) => panic!(
                "Cannot join node value {:?} with {:?} - they have different types.",
                val1, val2
            ),
        }
    }
}

// == NodeValue ==

#[derive(Clone, Eq, PartialEq)]
pub struct NodeValue {
    pub value: AbstractValue,
    pub source: NodeValueSource,
}

#[allow(dead_code)]
#[derive(Clone, Eq, PartialEq, Hash)]
pub enum NodeValueSource {
    Unknown,
    Node(Node),
    // child NodeValueSource are not unknown
    Phi(Phi, Vec<Rc<NodeValueSource>>),
}

impl fmt::Debug for NodeValueSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeValueSource::Unknown => write!(f, ""),
            NodeValueSource::Node(node) => write!(f, "{}", node.debug_fmt().short(true)),
            NodeValueSource::Phi(_phi, preds) => write!(f, "phi{{{:?}}}", preds),
        }
    }
}

impl NodeValueSource {
    pub fn mode(&self) -> Option<Mode> {
        match self {
            NodeValueSource::Unknown => None,
            NodeValueSource::Node(node) => Some(node.mode()),
            NodeValueSource::Phi(_phi, preds) => Some(preds.first().unwrap().mode().unwrap()),
        }
    }
}

impl NodeValueSource {
    pub fn is_unknown(&self) -> bool {
        match self {
            NodeValueSource::Unknown => true,
            _ => false,
        }
    }
}

impl From<Option<Node>> for NodeValueSource {
    fn from(opt: Option<Node>) -> NodeValueSource {
        match opt {
            Some(node) => NodeValueSource::Node(node),
            None => NodeValueSource::Unknown,
        }
    }
}

impl NodeValue {
    pub fn new(value: AbstractValue, source: Option<Node>) -> Self {
        assert!(
            match (&value, &source) {
                (AbstractValue::Pointer(..), node) => {
                    node.map(|n| n.mode() == Mode::P()).unwrap_or(true)
                }
                (AbstractValue::Tarval(tv), node) => {
                    !tv.is_unknown()
                        && tv.mode() != Mode::P()
                        && (tv.is_bad()
                            || node
                                // for `div` and `mod`, mode `T`uples is used.
                                .map(|n| n.mode() == tv.mode() || n.mode() == Mode::T())
                                .unwrap_or(true))
                }
            },
            "{:?} {:?}",
            value,
            source
        );
        Self {
            value,
            source: source.into(),
        }
    }

    pub fn value(value: AbstractValue) -> Self {
        Self::new(value, None)
    }

    pub fn zero(mode: Mode) -> Self {
        Self::new(
            if mode.is_pointer() {
                Pointer::null().into()
            } else {
                Tarval::zero(mode).into()
            },
            None,
        )
    }

    pub fn from_known_tarval(val: Tarval, mode: Mode, source: Option<Node>) -> Self {
        if mode.is_pointer() {
            if val.is_bad() {
                // still cannot point to created objects in the current method
                NodeValue::new(Pointer::to(MemoryArea::external()).into(), source)
            } else if val.is_zero() {
                NodeValue::new(Pointer::null().into(), source)
            } else {
                panic!(
                    "Got unexpected non-bad pointer val {:?} and expected mode {:?}",
                    val, mode
                )
            }
        } else {
            NodeValue::new(val.into(), source)
        }
    }

    pub fn non_const_node(source: Node) -> Self {
        Self::from_known_tarval(Tarval::bad(), source.mode(), Some(source))
    }

    pub fn non_const_val(mode: Mode, mem: MemoryArea) -> NodeValue {
        if mode.is_pointer() {
            Self::value(Pointer::to(mem).into())
        } else {
            Self::value(Tarval::bad().into())
        }
    }

    pub fn points_to(&self) -> MemoryArea {
        match &self.value {
            AbstractValue::Pointer(ptr) => ptr.target.clone(),
            _ => MemoryArea::empty(),
        }
    }

    pub fn tarval(&self) -> Tarval {
        match &self.value {
            AbstractValue::Tarval(tv) => *tv,
            AbstractValue::Pointer(ptr) if ptr.is_null() => Tarval::zero(Mode::P()),
            AbstractValue::Pointer(_ptr) => Tarval::bad(),
        }
    }

    pub fn is_tarval(&self) -> bool {
        match &self.value {
            AbstractValue::Tarval(..) => true,
            _ => false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        match &self.value {
            AbstractValue::Pointer(..) => true,
            _ => false,
        }
    }

    pub fn as_pointer(&self) -> Option<&Pointer> {
        match &self.value {
            AbstractValue::Pointer(ptr) => Some(ptr),
            _ => None,
        }
    }

    pub fn into_updated_source_ex(self, source: Node) -> Self {
        Self {
            source: Some(source).into(),
            value: self.value,
        }
    }

    pub fn source_or_some_ex(&self, or_source: Node) -> Node {
        match self.source {
            NodeValueSource::Node(node) => node,
            _ => or_source,
        }
    }
}

impl fmt::Debug for NodeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.value)?;
        match &self.source {
            NodeValueSource::Node(..) | NodeValueSource::Phi(..) => {
                write!(f, "←{:?}", self.source)?;
            }
            _ => {}
        }
        Ok(())
    }
}

impl Lattice for NodeValue {
    fn is_progression_of(&self, _other: &Self) -> bool {
        // todo
        true
    }

    fn join(&self, other: &Self, context: &JoinContext) -> Self {
        let source = match context {
            /*JoinContext::Phi {
                phi,
                is_self_initial,
                other_pred_idx: _,
            } => {
                if *is_self_initial {
                    match (&self.source, &other.source) {
                        (NodeValueSource::Unknown, _) | (_, NodeValueSource::Unknown) => {
                            // or: if self.source.block is no dominator of phi.preds(0)
                            // or if source.block is no dom of phi.preds(idx)
                            NodeValueSource::Unknown
                        }
                        _ => {
                            let preds =
                                vec![Rc::new(self.source.clone()), Rc::new(other.source.clone())];
                            NodeValueSource::Phi(*phi, preds)
                        }
                    }
                } else {
                    unimplemented!()
                    / * match (&self.source, other.source) {
                        (NodeValueSource::Phi(phi, preds), _) => {
                            //
                        }
                    }* /
                }
            }*/
            _ => match (&self.source, &other.source) {
                (NodeValueSource::Node(node1), NodeValueSource::Node(node2)) if node1 == node2 => {
                    NodeValueSource::Node(*node1)
                }
                _ => NodeValueSource::Unknown,
            },
        };

        NodeValue {
            value: self.value.join(&other.value, context),
            source,
        }
    }
}
