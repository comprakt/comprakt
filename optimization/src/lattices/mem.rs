use super::{JoinContext, Lattice};
use libfirm_rs::nodes::{Node, NodeDebug};
use std::{collections::HashSet, fmt};

// == MemoryArea ==

#[derive(Clone, PartialEq, Eq)]
pub struct MemoryArea {
    allocators: HashSet<Node>,
    external: bool,
    arbitrary: bool,
}

impl MemoryArea {
    pub fn empty() -> Self {
        Self {
            allocators: HashSet::new(),
            external: false,
            arbitrary: false,
        }
    }

    pub fn single(allocator: Node) -> Self {
        let mut allocators = HashSet::new();
        allocators.insert(allocator);
        Self {
            allocators,
            external: false,
            arbitrary: false,
        }
    }

    pub fn external() -> Self {
        Self {
            allocators: HashSet::new(),
            external: true,
            arbitrary: false,
        }
    }

    #[allow(dead_code)]
    pub fn arbitrary() -> Self {
        Self {
            allocators: HashSet::new(),
            external: true,
            arbitrary: true,
        }
    }

    pub fn intersects(&self, other: &Self) -> bool {
        self.arbitrary
            || other.arbitrary
            || (self.external && other.external)
            || !self.allocators.is_disjoint(&other.allocators)
    }

    pub fn is_empty(&self) -> bool {
        self.allocators.is_empty() && !self.external && !self.arbitrary
    }

    pub fn join_mut(&mut self, other: &Self) -> bool /* changed */ {
        let old = (self.allocators.len(), self.external, self.arbitrary);
        self.allocators.extend(&other.allocators);
        self.external |= other.external;
        self.arbitrary |= other.arbitrary;
        old != (self.allocators.len(), self.external, self.arbitrary)
    }

    pub fn has_external(&self) -> bool {
        self.external
    }
}

impl Lattice for MemoryArea {
    fn is_progression_of(&self, other: &Self) -> bool {
        self.allocators.is_superset(&other.allocators) && (!other.external || self.external)
    }

    fn join(&self, other: &Self, _context: &mut JoinContext) -> Self {
        Self {
            allocators: &self.allocators | &other.allocators,
            external: self.external || other.external,
            arbitrary: self.arbitrary || other.arbitrary,
        }
    }
}

impl fmt::Debug for MemoryArea {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.allocators
                .iter()
                .map(|allocator| format!("{}", allocator.debug_fmt().short(true)))
                .chain(if self.external {
                    vec!["*".to_owned()]
                } else {
                    vec![]
                })
                .chain(if self.arbitrary {
                    vec!["!".to_owned()]
                } else {
                    vec![]
                })
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

// == Pointer ==

#[derive(Clone, PartialEq, Eq)]
pub struct Pointer {
    pub target: MemoryArea,
    pub can_be_null: bool,
}

#[allow(clippy::wrong_self_convention)]
impl Pointer {
    pub fn to_null_and(target: MemoryArea) -> Self {
        Self {
            target,
            can_be_null: true,
        }
    }

    pub fn to(target: MemoryArea) -> Self {
        Self {
            target,
            can_be_null: false,
        }
    }

    pub fn null() -> Self {
        Self {
            target: MemoryArea::empty(),
            can_be_null: true,
        }
    }

    pub fn is_null(&self) -> bool {
        self.can_be_null && self.target.is_empty()
    }

    pub fn is_null_or_empty(&self) -> bool {
        self.target.is_empty()
    }

    pub fn eq(&self, other: &Self) -> Option<bool> {
        if self.is_null() && other.is_null() {
            return Some(true);
        }

        if !(self.can_be_null && other.can_be_null || self.target.intersects(&other.target)) {
            // mems are disjoint and one of them cannot be null.
            return Some(false);
        }

        None
    }
}

impl Lattice for Pointer {
    fn is_progression_of(&self, other: &Self) -> bool {
        self.target.is_progression_of(&other.target) && (!other.can_be_null || self.can_be_null)
    }

    fn join(&self, other: &Self, context: &mut JoinContext) -> Self {
        Self {
            target: self.target.join(&other.target, context),
            can_be_null: self.can_be_null || other.can_be_null,
        }
    }
}

impl fmt::Debug for Pointer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let target_str = format!("{:?}", self.target);
        write!(
            f,
            "{{{}{}}}",
            target_str,
            if self.can_be_null {
                if target_str.is_empty() {
                    &"null"
                } else {
                    &", null"
                }
            } else {
                &""
            }
        )
    }
}
