mod heap;
pub use self::heap::*;
use libfirm_rs::{
    nodes::{Node, NodeDebug},
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
#[derive(Clone, PartialEq, Eq)]
pub struct ConstantFoldingLattice {
    reachable: bool,
    value: Val,
}

impl ConstantFoldingLattice {
    pub fn new(reachable: bool, value: Val) -> Self {
        Self { reachable, value }
    }

    pub fn start() -> Self {
        Self {
            reachable: false,
            value: Val::start(),
        }
    }

    pub fn value(&self) -> &Val {
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

// == Val ==

#[derive(Eq, PartialEq, Clone)]
pub enum NodeValue {
    Tarval(Tarval),
    Pointer(Pointer),
}

impl fmt::Debug for NodeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeValue::Pointer(ptrs) => write!(f, "{:?}", ptrs),
            NodeValue::Tarval(val) => write!(f, "{:?}", val),
        }
    }
}

impl From<Pointer> for NodeValue {
    fn from(pointer: Pointer) -> Self {
        NodeValue::Pointer(pointer)
    }
}

impl From<Tarval> for NodeValue {
    fn from(val: Tarval) -> Self {
        NodeValue::Tarval(val)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Val {
    NoInfoYet,
    NodeValue(NodeValue, Option<Node>),
    Heap(Rc<Heap>),
    Tuple(Box<Val>, Box<Val>),
    Invalid,
}

impl Val {
    pub fn start() -> Val {
        Val::NoInfoYet
    }

    pub fn zero(mode: Mode, from: Option<Node>) -> Val {
        Val::NodeValue(
            if mode.is_pointer() {
                Pointer::null().into()
            } else {
                Tarval::zero(mode).into()
            },
            from,
        )
    }

    pub fn from_tarval_initially(val: Tarval, mode: Mode, from: Option<Node>) -> Val {
        match val.kind() {
            TarvalKind::Bad if mode.is_pointer() => {
                Val::NodeValue(Pointer::to(MemoryArea::unrestricted()).into(), from)
            }
            TarvalKind::Unknown => Val::NoInfoYet,
            _ if mode.is_pointer() && val.is_zero() => Val::NodeValue(Pointer::null().into(), from),
            _ => Val::NodeValue(val.into(), from),
        }
    }

    pub fn from_tarval_internal(val: Tarval, from: Option<Node>) -> Val {
        match val.kind() {
            TarvalKind::Unknown => Val::NoInfoYet,
            _ => Val::NodeValue(val.into(), from),
        }
    }

    pub fn tuple(val1: Val, val2: Val) -> Val {
        Val::Tuple(Box::new(val1), Box::new(val2))
    }

    pub fn tuple_1(&self) -> &Val {
        match self {
            Val::Tuple(t1, _t2) => &t1,
            Val::NoInfoYet => &Val::NoInfoYet,
            val => panic!("Invalid data type {:?}", val),
        }
    }

    pub fn tuple_2(&self) -> &Val {
        match self {
            Val::Tuple(_t1, t2) => &t2,
            Val::NoInfoYet => &Val::NoInfoYet,
            val => panic!("Invalid data type {:?}", val),
        }
    }

    pub fn expect_node_value_or_no_info(&self) -> Option<(&NodeValue, Option<Node>)> {
        match self {
            Val::NodeValue(val, node) => Some((val, *node)),
            Val::NoInfoYet => None,
            _ => panic!("Expected NodeValue, but got: {:?}", self),
        }
    }

    pub fn points_to(&self) -> MemoryArea {
        match self {
            Val::NodeValue(NodeValue::Pointer(ptr), _) => ptr.target.clone(),
            _ => MemoryArea::empty(),
        }
    }
}

impl Lattice for Val {
    fn is_progression_of(&self, other: &Self) -> bool {
        fn is_option_progression_of(option: &Option<Node>, base: &Option<Node>) -> bool {
            option == base || base == &None
        }

        use self::Val::*;
        match (self, other) {
            (_, NoInfoYet) => true,
            (
                NodeValue(self::NodeValue::Tarval(val1), n1),
                NodeValue(self::NodeValue::Tarval(val2), n2),
            ) => val1.lattice_eq(*val2) && is_option_progression_of(n1, n2),
            (
                NodeValue(self::NodeValue::Pointer(p1), n1),
                NodeValue(self::NodeValue::Pointer(p2), n2),
            ) => p1.is_progression_of(p2) && is_option_progression_of(n1, n2),
            (Heap(heap1), Heap(heap2)) => heap1.is_progression_of(heap2),
            (Tuple(a1, a2), Tuple(b1, b2)) => a1.is_progression_of(b1) && a2.is_progression_of(b2),
            _ => false,
        }
    }

    fn join(&self, other: &Self) -> Self {
        fn join_option(n1: &Option<Node>, n2: &Option<Node>) -> Option<Node> {
            if n1 == n2 {
                *n1
            } else {
                None
            }
        }

        use self::{NodeValue::*, Val::*};
        match (self, other) {
            (NoInfoYet, arg2) => arg2.clone(),
            (arg1, NoInfoYet) => arg1.clone(),
            (NodeValue(val1, n1), NodeValue(val2, n2)) => NodeValue(
                match (val1, val2) {
                    (Tarval(val1), Tarval(val2)) => {
                        return Val::from_tarval_internal(val1.join(*val2), join_option(n1, n2));
                    }
                    (Pointer(ps1), Pointer(ps2)) => Pointer(ps1.join(ps2)),
                    (Pointer(ps), Tarval(tval)) | (Tarval(tval), Pointer(ps)) if ps.is_null() => {
                        log::debug!(
                            "Join {:?} with {:?} that should happen\
                             only when load store is disabled",
                            ps,
                            tval
                        );
                        Tarval(*tval)
                    }
                    (val1, val2) => panic!(
                        "Cannot join node value {:?} with {:?} - they have different types.",
                        val1, val2
                    ),
                },
                join_option(n1, n2),
            ),
            (Heap(heap1), Heap(heap2)) => Heap(Rc::new(heap1.join(heap2))),
            (Tuple(a1, a2), Tuple(b1, b2)) => Val::tuple(a1.join(b1), a2.join(b2)),
            (val1, val2) => panic!(
                "Cannot join value {:?} with {:?} - they have different types.",
                val1, val2
            ),
        }
    }
}

impl fmt::Debug for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::NodeValue(val, val_source) => write!(
                f,
                "{:?}{}",
                val,
                if let Some(val_source) = val_source {
                    format!(" <from {}>", val_source.debug_fmt().short(true))
                } else {
                    "".to_string()
                }
            ),
            Val::NoInfoYet => write!(f, "No info"),
            Val::Heap(heap) => write!(f, "{:?}", heap),
            Val::Tuple(..) => write!(f, "Tuple"),
            Val::Invalid => write!(f, "Invalid"),
        }
    }
}
