mod heap;
pub use self::heap::*;

use libfirm_rs::{Mode, Tarval, TarvalKind};
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
    Pointer(Pointer),
    Heap(Rc<Heap>),
    Tuple(Box<Val>, Box<Val>),
    Invalid,
}

impl Val {
    pub fn start() -> Val {
        Val::NoInfoYet
    }

    pub fn from_tarval_initially(val: Tarval, mode: Mode) -> Val {
        match val.kind() {
            TarvalKind::Bad if mode.is_pointer() => {
                Val::Pointer(Pointer::to(MemoryArea::unrestricted()))
            }
            TarvalKind::Unknown => Val::NoInfoYet,
            _ if mode.is_pointer() && val.is_zero() => Val::Pointer(Pointer::null()),
            _ => Val::Tarval(val),
        }
    }

    pub fn from_tarval_internal(val: Tarval) -> Val {
        match val.kind() {
            TarvalKind::Unknown => Val::NoInfoYet,
            _ => Val::Tarval(val),
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

    /*pub fn update_dom_depth(self, dom_depth: usize) -> Self {
        match self {
            //Val::Heap(heap) => heap.update_dom_depth
            Val::NoInfoYet | Val::Tarval(_) => self,
            Val::Heap(heap) => {
                let mut heap = (&*heap).clone();
                heap.update_dom_depth(dom_depth);
                Val::Heap(Rc::new(heap))
            }
            Val::Pointer(ptrs) => {
                let mut ptrs = ptrs.clone();
                ptrs.update_dom_depth(dom_depth);
                Val::Pointer(ptrs)
            }
            val => panic!("{:?} is not supported.", val),
        }
    }*/
}

impl Lattice for Val {
    fn is_progression_of(&self, other: &Self) -> bool {
        use self::Val::*;
        match (self, other) {
            // todo
            //(NonConst(_), _) => true,
            (_, NoInfoYet) => true,
            (Tarval(val1), Tarval(val2)) => val1.lattice_eq(*val2),
            (Pointer(ps1), Pointer(ps2)) => ps1.is_progression_of(ps2),
            (Heap(heap1), Heap(heap2)) => heap1.is_progression_of(heap2),
            (Tuple(a1, a2), Tuple(b1, b2)) => a1.is_progression_of(b1) && a2.is_progression_of(b2),
            _ => false,
        }
    }

    fn join(&self, other: &Self) -> Self {
        use self::Val::*;
        match (self, other) {
            //(NonConst(ptrs1), NonConst(ptrs2)) => NonConst(ptrs1.join(ptrs2)),
            //| NonConstant) => NonConstant,
            (NoInfoYet, arg2) => arg2.clone(),
            (arg1, NoInfoYet) => arg1.clone(),
            (Tarval(val1), Tarval(val2)) => {
                Val::from_tarval_internal(val1.join(*val2))
                /*if val1.lattice_eq(*val2) {
                    Tarval(*val1)
                } else {
                    NonConstant(Pointer::new_empty())
                }*/
            }
            (Pointer(ps1), Pointer(ps2)) => Pointer(ps1.join(ps2)),
            (Heap(heap1), Heap(heap2)) => Heap(Rc::new(heap1.join(heap2))),
            (Tuple(a1, a2), Tuple(b1, b2)) => Val::tuple(a1.join(b1), a2.join(b2)),
            (Pointer(ps), Tarval(tval)) | (Tarval(tval), Pointer(ps)) if ps.is_null() => {
                log::debug!(
                    "Join {:?} with {:?} that should happen only when load store is disabled",
                    ps,
                    tval
                );
                Tarval(*tval)
            }
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
            Val::Pointer(ptrs) => write!(f, "{:?}", ptrs),
            Val::Tarval(val) => write!(f, "{:?}", val),
            //Val::NonConstant => write!(f, "Not Const"),
            Val::NoInfoYet => write!(f, "No info"),
            Val::Heap(heap) => write!(f, "{:?}", heap),
            Val::Tuple(..) => write!(f, "Tuple"),
            Val::Invalid => write!(f, "Invalid"),
        }
    }
}
