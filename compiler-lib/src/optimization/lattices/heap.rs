use super::{Lattice, Val};
use libfirm_rs::{
    nodes::Node,
    types::{ClassTy, TyTrait},
    Entity, Tarval,
};
use std::{
    collections::{HashMap, HashSet},
    fmt,
    rc::Rc,
};

// == Pointer ==

/// Represents a symbolic pointer. Two equal points point to the same memory.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Pointer {
    p: usize,
    recent: bool,
}

impl Pointer {
    pub fn new(p: usize) -> Self {
        Self { p, recent: false }
    }

    pub fn recent(self) -> bool {
        self.recent
    }

    pub fn as_recent(self) -> Self {
        Self {
            p: self.p,
            recent: true,
        }
    }
}

impl fmt::Debug for Pointer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}{}", self.p, if self.recent { "r" } else { "" })
    }
}

// == PointerSet ==

#[derive(Clone, PartialEq, Eq)]
pub struct PointerSet {
    pointers: HashSet<Pointer>,
    can_be_null: bool,
}

impl PointerSet {
    pub fn can_be_null(&self) -> bool {
        self.can_be_null
    }

    pub fn with_null(&self) -> PointerSet {
        PointerSet {
            pointers: self.pointers.clone(),
            can_be_null: true,
        }
    }

    pub fn as_single_ignoring_null(&self) -> Option<Pointer> {
        if self.pointers.is_empty() {
            None
        } else {
            self.pointers.iter().next().cloned()
        }
    }

    pub fn pointers(&self) -> &HashSet<Pointer> {
        &self.pointers
    }
}

impl From<Pointer> for PointerSet {
    fn from(p: Pointer) -> PointerSet {
        let mut pointers = HashSet::new();
        pointers.insert(p);
        Self {
            pointers,
            can_be_null: false,
        }
    }
}

impl Lattice for PointerSet {
    fn is_progression_of(&self, other: &PointerSet) -> bool {
        self.pointers.is_superset(&other.pointers) && (!other.can_be_null || self.can_be_null)
    }

    fn join(&self, other: &PointerSet) -> PointerSet {
        PointerSet {
            pointers: self.pointers.union(&other.pointers).cloned().collect(),
            can_be_null: self.can_be_null || other.can_be_null,
        }
    }
}

impl fmt::Debug for PointerSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.pointers
                .iter()
                .map(|o| format!("{:?}", o))
                .chain(if self.can_be_null {
                    vec!["null".to_owned()]
                } else {
                    vec![]
                })
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

// == HeapVal ==

#[derive(Clone, PartialEq, Eq)]
pub struct Heap {
    objects: HashMap<Pointer, Rc<ObjectInfo>>,
}

impl fmt::Debug for Heap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (ptr, obj_info) in &self.objects {
            writeln!(f, "{:?} = {:?}", ptr, obj_info)?
        }
        Ok(())
    }
}

impl Heap {
    pub fn start() -> Self {
        Self {
            objects: HashMap::new(),
        }
    }

    pub fn new_obj(&mut self, p: Pointer, obj_ty: ClassTy) {
        let obj_info = Rc::new(ObjectInfo::new(obj_ty));
        self.objects.insert(p, obj_info);
    }

    pub fn exists(&self, ptr: Pointer) -> bool {
        self.objects.contains_key(&ptr)
    }

    fn is_ptr_class(&self, ptr: Pointer) -> bool {
        !ptr.recent() && self.objects.contains_key(&ptr.as_recent())
    }

    pub fn reset(&mut self) {
        self.objects = self
            .objects
            .iter()
            .map(|(ptr, obj_info)| (*ptr, Rc::new(obj_info.resetted())))
            .collect();
    }

    pub fn update_field(
        &mut self,
        _ptr: Node,
        ptrs: Option<&PointerSet>,
        field: Entity,
        val: &Val,
    ) {
        match ptrs.and_then(|ptrs| ptrs.as_single_ignoring_null()) {
            Some(obj) if !self.is_ptr_class(obj) => {
                let mut obj_info = (**self
                    .objects
                    .get(&obj)
                    .unwrap_or_else(|| panic!("{:?} to have {:?}", self, obj)))
                .clone();
                obj_info.update_field(field, val);
                self.objects
                    .insert(obj, Rc::new(obj_info))
                    .expect("to exist");
            }
            _ => {
                // fallback: clear all info for now
                self.reset()
            }
        }
    }

    pub fn lookup_field(&mut self, _ptr: Node, ptrs: Option<&PointerSet>, field: Entity) -> Val {
        if let Some(ptrs) = ptrs {
            let result = Val::join_many(ptrs.pointers().iter().map(|p| {
                self.objects
                    .get(&p)
                    .unwrap_or_else(|| panic!("{:?} to have {:?}", self, p))
                    .lookup_field(field)
            }));
            result.unwrap_or(Val::NonConstant)
        } else {
            Val::NonConstant
        }
    }
}

impl Lattice for Heap {
    fn is_progression_of(&self, other: &Self) -> bool {
        for (p, obj_info) in &self.objects {
            if let Some(other_obj_info) = other.objects.get(&p) {
                if !obj_info.is_progression_of(other_obj_info) {
                    return false;
                }
            }
        }
        true
    }

    fn join(&self, other: &Self) -> Self {
        let mut objects = self.objects.clone();
        for (p, obj_info) in other.objects.iter() {
            if let Some(other_obj_info) = objects.get(&p) {
                objects.insert(*p, Rc::new(obj_info.join(other_obj_info)));
            } else {
                objects.insert(*p, Rc::clone(&obj_info));
            }
        }
        Heap { objects }
    }
}

// == ObjectInfo ==

#[derive(Clone, PartialEq, Eq)]
pub struct ObjectInfo {
    ty: ClassTy,
    // stores a value for each field
    fields: Vec<Val>,
}

impl fmt::Debug for ObjectInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{{", self.ty.name_string())?;
        let mut first = true;
        for (val, field) in self.fields.iter().zip(self.ty.fields()) {
            let name = field.name_string();
            let field_name = name.split('.').last().unwrap();
            if !first {
                write!(f, ", ")?;
            }
            write!(f, "{}: {:?}", field_name, val)?;
            first = false;
        }
        write!(f, "}}")
    }
}

impl ObjectInfo {
    pub fn new(ty: ClassTy) -> Self {
        Self {
            ty,
            fields: ty
                .fields()
                .map(|field| {
                    let mode = field.ty().mode();
                    Val::Tarval(Tarval::zero(mode))
                })
                .collect(),
        }
    }

    pub fn new_non_const(ty: ClassTy) -> Self {
        Self {
            ty,
            fields: ty.fields().map(|_field| Val::NonConstant).collect(),
        }
    }

    pub fn resetted(&self) -> Self {
        Self::new_non_const(self.ty)
    }

    pub fn update_field(&mut self, field: Entity, val: &Val) {
        let field_idx = self.ty.idx_of_field(field);
        self.fields[field_idx] = val.clone();
    }

    pub fn lookup_field(&self, field: Entity) -> &Val {
        let idx = self.ty.idx_of_field(field);
        &self.fields[idx]
    }
}

impl Lattice for ObjectInfo {
    fn is_progression_of(&self, other: &Self) -> bool {
        assert!(self.ty == other.ty);

        for (field_idx, field_val) in self.fields.iter().enumerate() {
            if !field_val.is_progression_of(&other.fields[field_idx]) {
                return false;
            }
        }
        true
    }

    fn join(&self, other: &Self) -> Self {
        assert!(self.ty == other.ty);

        let mut updated_fields = Vec::with_capacity(self.fields.len());
        for (field_idx, field_val) in self.fields.iter().enumerate() {
            updated_fields.push(field_val.join(&other.fields[field_idx]));
        }
        Self {
            ty: self.ty,
            fields: updated_fields,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_lattice_bin<T: Lattice + std::fmt::Debug>(l1: T, l2: T) {
        if l1 == l2 {
            assert!(l1.is_progression_of(&l2));
            assert!(l2.is_progression_of(&l1));
        }

        assert_eq!(l1.join(&l2), l2.join(&l1));
        assert!(l1.join(&l2).is_progression_of(&l1));
        assert!(l1.join(&l2).is_progression_of(&l2));

        if l1.is_progression_of(&l2) && l2.is_progression_of(&l1) {
            assert!(l1 == l2);
        }
        if l1.join(&l2) == l1 {
            assert!(l1.is_progression_of(&l2));
        }
    }

    fn test_lattice<T: Lattice + std::fmt::Debug + Clone>(elements: &[T]) {
        for (a, b) in elements.iter().zip(elements.iter()) {
            test_lattice_bin(a.clone(), b.clone());
        }
    }

    #[test]
    fn test_pointer_set() {
        let p1 = Pointer::new(0);
        assert_eq!(p1.p, 0);
        assert_eq!(p1.recent, false);

        let p2 = Pointer::new(1).as_recent();
        assert_eq!(p2.p, 1);
        assert_eq!(p2.recent, true);

        let p1_set: PointerSet = p1.into();
        assert_eq!(p1_set.can_be_null, false);
        assert_eq!(p1_set.pointers, [p1].iter().cloned().collect());

        let p2_set: PointerSet = p2.into();

        let p12_set = p1_set.join(&p2_set);
        assert_eq!(p12_set.can_be_null, false);
        assert_eq!(p12_set.pointers, [p1, p2].iter().cloned().collect());

        assert!(p12_set.is_progression_of(&p1_set));

        test_lattice(&[p1_set, p2_set, p12_set]);
    }
}
