mod heap;
pub use self::heap::*;
use libfirm_rs::{
    nodes::{Node, NodeDebug, NodeTrait},
    Mode, Tarval, TarvalKind,
};
use std::{fmt, rc::Rc};

pub trait Lattice: Eq + Clone {
    fn is_progression_of(&self, other: &Self) -> bool;
    fn join(&self, other: &Self) -> Self;

    fn join_many<'t, I>(vals: I) -> Option<Self>
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
    }
}

// == ConstantFoldingLattice ==

// The lattice used for constant folding.
// TODO get rid of it and rename no_info_yet to unreachable
#[derive(Clone, PartialEq, Eq)]
pub struct ConstantFoldingLattice {
    reachable: bool,
    value: NodeLattice,
}

impl ConstantFoldingLattice {
    pub fn new(reachable: bool, value: NodeLattice) -> Self {
        Self { reachable, value }
    }

    pub fn start() -> Self {
        Self {
            reachable: false,
            value: NodeLattice::start(),
        }
    }

    pub fn value(&self) -> &NodeLattice {
        &self.value
    }

    pub fn reachable(&self) -> bool {
        self.reachable
    }
}

impl Lattice for ConstantFoldingLattice {
    fn is_progression_of(&self, other: &Self) -> bool {
        self.value.is_progression_of(&other.value) && (self.reachable || !other.reachable)
    }

    fn join(&self, other: &Self) -> Self {
        Self {
            value: self.value.join(&other.value),
            reachable: self.reachable,
        }
    }
}

impl fmt::Debug for ConstantFoldingLattice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {:?}",
            if self.reachable { "⮑" } else { "🛇" },
            self.value,
        )
    }
}

// == NodeLattice ==

#[derive(Clone, PartialEq, Eq)]
pub enum NodeLattice {
    NoInfoYet,
    Value(NodeValue),
    Heap(Rc<Heap>),
    Tuple(Box<NodeLattice>, Box<NodeLattice>),
    Invalid,
}

impl NodeLattice {
    pub fn start() -> NodeLattice {
        NodeLattice::NoInfoYet
    }

    pub fn tuple(val1: NodeLattice, val2: NodeLattice) -> NodeLattice {
        NodeLattice::Tuple(Box::new(val1), Box::new(val2))
    }

    pub fn tuple_1(&self) -> &NodeLattice {
        match self {
            NodeLattice::Tuple(t1, _t2) => &t1,
            NodeLattice::NoInfoYet => &NodeLattice::NoInfoYet,
            val => panic!("Invalid data type {:?}", val),
        }
    }

    pub fn tuple_2(&self) -> &NodeLattice {
        match self {
            NodeLattice::Tuple(_t1, t2) => &t2,
            NodeLattice::NoInfoYet => &NodeLattice::NoInfoYet,
            val => panic!("Invalid data type {:?}", val),
        }
    }

    pub fn expect_value_or_no_info(&self) -> Option<&NodeValue> {
        match self {
            NodeLattice::Value(val) => Some(val),
            NodeLattice::NoInfoYet => None,
            _ => panic!("Expected NodeValue, but got: {:?}", self),
        }
    }

    fn from_tarval_optional_node(val: Tarval, mode: Mode, source: Option<Node>) -> Self {
        match val.kind() {
            TarvalKind::Unknown => NodeLattice::NoInfoYet,
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
            (_, NoInfoYet) => true,
            (Value(v1), Value(v2)) => v1.is_progression_of(v2),
            (Heap(heap1), Heap(heap2)) => heap1.is_progression_of(heap2),
            (Tuple(a1, a2), Tuple(b1, b2)) => a1.is_progression_of(b1) && a2.is_progression_of(b2),
            _ => false,
        }
    }

    fn join(&self, other: &Self) -> Self {
        use self::NodeLattice::*;
        match (self, other) {
            (NoInfoYet, arg2) => arg2.clone(),
            (arg1, NoInfoYet) => arg1.clone(),
            (Value(val1), Value(val2)) => Value(val1.join(val2)),
            (Heap(heap1), Heap(heap2)) => Heap(Rc::new(heap1.join(heap2))),
            (Tuple(a1, a2), Tuple(b1, b2)) => NodeLattice::tuple(a1.join(b1), a2.join(b2)),
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
            NodeLattice::NoInfoYet => write!(f, "No info"),
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

    fn join(&self, other: &Self) -> Self {
        use self::AbstractValue::*;
        match (self, other) {
            (Tarval(val1), Tarval(val2)) => Tarval(val1.join(*val2)),
            (Pointer(ps1), Pointer(ps2)) => Pointer(ps1.join(ps2)),
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
    pub source: Option<Node>,
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
        Self { value, source }
    }

    /*pub fn zero_from(mode: Mode, from: Node) -> Self {
        NodeLattice::Value(
            if mode.is_pointer() {
                Pointer::null().into()
            } else {
                Tarval::zero(mode).into()
            },
            from,
        )
    }*/

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
                NodeValue::new(Pointer::to(MemoryArea::unrestricted()).into(), source)
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

    /*
        pub fn from_tarval_internal(val: Tarval, from: Option<Node>) -> NodeLattice {
            match val.kind() {
                TarvalKind::Unknown => NodeLattice::NoInfoYet,
                _ => NodeLattice::Value(val.into(), from),
            }
        }
    */
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

    pub fn into_updated_source(self, source: Node) -> Self {
        Self {
            source: Some(source),
            value: self.value,
        }
    }

    pub fn source_or_some(&self, or_source: Node) -> Node {
        self.source.unwrap_or(or_source)
    }
}

impl fmt::Debug for NodeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.value)?;
        if let Some(source) = self.source {
            write!(f, "←{}", source.debug_fmt().short(true))?;
        }
        Ok(())
    }
}

impl Lattice for NodeValue {
    fn is_progression_of(&self, _other: &Self) -> bool {
        // todo
        true
    }

    fn join(&self, other: &Self) -> Self {
        fn join_option(n1: &Option<Node>, n2: &Option<Node>) -> Option<Node> {
            if n1 == n2 {
                *n1
            } else {
                None
            }
        }

        NodeValue {
            value: self.value.join(&other.value),
            source: join_option(&self.source, &other.source),
        }
    }
}
