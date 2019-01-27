mod heap;
pub use self::heap::*;

use libfirm_rs::{Tarval, TarvalKind};
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

#[derive(Clone, PartialEq, Eq)]
pub enum Val {
    NoInfoYet,
    Tarval(Tarval),
    ObjPointers(PointerSet),
    ArrPointers(PointerSet),
    Heap(Rc<Heap>),
    Tuple(Box<Val>, Box<Val>),
    NonConstant,
}

impl Val {
    pub fn start() -> Val {
        Val::NoInfoYet
    }

    pub fn from_tarval(val: Tarval) -> Val {
        match val.kind() {
            TarvalKind::Bad => Val::NonConstant,
            TarvalKind::Unknown => Val::NoInfoYet,
            _ => Val::Tarval(val),
        }
    }

    pub fn tuple(val1: Val, val2: Val) -> Val {
        Val::Tuple(Box::new(val1), Box::new(val2))
    }

    pub fn tuple_1(&self) -> &Val {
        match self {
            Val::NonConstant => &Val::NonConstant,
            Val::Tuple(t1, _t2) => &t1,
            Val::NoInfoYet => &Val::NoInfoYet,
            val => panic!("Invalid data type {:?}", val),
        }
    }

    pub fn tuple_2(&self) -> &Val {
        match self {
            Val::NonConstant => &Val::NonConstant,
            Val::Tuple(_t1, t2) => &t2,
            Val::NoInfoYet => &Val::NoInfoYet,
            val => panic!("Invalid data type {:?}", val),
        }
    }
}

impl Lattice for Val {
    fn is_progression_of(&self, other: &Self) -> bool {
        use self::Val::*;
        match (self, other) {
            (NonConstant, _) | (_, NoInfoYet) => true,
            (Tarval(val1), Tarval(val2)) => val1.lattice_eq(*val2),
            (ObjPointers(ps), Tarval(val)) => ps.can_be_null() && val.is_zero(),
            (ArrPointers(ps), Tarval(val)) => ps.can_be_null() && val.is_zero(),
            (ObjPointers(ps1), ObjPointers(ps2)) => ps1.is_progression_of(ps2),
            (ArrPointers(ps1), ArrPointers(ps2)) => ps1.is_progression_of(ps2),
            (Heap(heap1), Heap(heap2)) => heap1.is_progression_of(heap2),
            (Tuple(a1, a2), Tuple(b1, b2)) => a1.is_progression_of(b1) && a2.is_progression_of(b2),
            _ => false,
        }
    }

    fn join(&self, other: &Self) -> Self {
        use self::Val::*;
        match (self, other) {
            (NonConstant, _) | (_, NonConstant) => NonConstant,
            (NoInfoYet, arg2) => arg2.clone(),
            (arg1, NoInfoYet) => arg1.clone(),
            (Tarval(val1), Tarval(val2)) => {
                if val1.lattice_eq(*val2) {
                    Tarval(*val1)
                } else {
                    NonConstant
                }
            }
            (Tarval(val), ObjPointers(ps)) | (ObjPointers(ps), Tarval(val)) if val.is_zero() => {
                ObjPointers(ps.with_null())
            }
            (Tarval(val), ArrPointers(ps)) | (ArrPointers(ps), Tarval(val)) if val.is_zero() => {
                ArrPointers(ps.with_null())
            }
            (ObjPointers(ps1), ObjPointers(ps2)) => ObjPointers(ps1.join(ps2)),
            (ArrPointers(ps1), ArrPointers(ps2)) => ArrPointers(ps1.join(ps2)),
            (Heap(heap1), Heap(heap2)) => Heap(Rc::new(heap1.join(heap2))),
            (Tuple(a1, a2), Tuple(b1, b2)) => Val::tuple(a1.join(b1), a2.join(b2)),
            _ => panic!("Cannot join values of different types."),
        }
    }
}

impl fmt::Debug for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::ObjPointers(ptrs) | Val::ArrPointers(ptrs) => write!(f, "{:?}", ptrs),
            Val::Tarval(val) => write!(f, "{:?}", val),
            Val::NonConstant => write!(f, "Not Const"),
            Val::NoInfoYet => write!(f, "No info"),
            Val::Heap(heap) => write!(f, "{:?}", heap),
            Val::Tuple(..) => write!(f, "Tuple"),
        }
    }
}
