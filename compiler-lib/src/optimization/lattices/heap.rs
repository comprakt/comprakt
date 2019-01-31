use super::{Lattice, Val};
use libfirm_rs::{
    nodes::{Node, NodeDebug},
    types::{ClassTy, PointerTy, Ty, TyTrait},
    Entity, Tarval,
};
use std::{
    collections::{HashMap, HashSet},
    fmt,
    rc::Rc,
};

// == MemoryArea ==

#[derive(Clone, PartialEq, Eq)]
pub struct MemoryArea {
    allocators: HashSet<Node>,
    unrestricted: bool,
}

impl MemoryArea {
    pub fn empty() -> Self {
        Self {
            allocators: HashSet::new(),
            unrestricted: false,
        }
    }

    pub fn single(allocator: Node) -> Self {
        let mut allocators = HashSet::new();
        allocators.insert(allocator);
        Self {
            allocators,
            unrestricted: false,
        }
    }

    pub fn unrestricted() -> Self {
        Self {
            allocators: HashSet::new(),
            unrestricted: true,
        }
    }

    pub fn intersects(&self, other: &Self) -> bool {
        (self.unrestricted && other.unrestricted) || !self.allocators.is_disjoint(&other.allocators)
    }

    pub fn is_empty(&self) -> bool {
        self.allocators.is_empty() && !self.unrestricted
    }

    pub fn join_mut(&mut self, other: &Self) {
        self.allocators.extend(&other.allocators);
        self.unrestricted |= other.unrestricted;
    }
}

impl Lattice for MemoryArea {
    fn is_progression_of(&self, other: &Self) -> bool {
        self.allocators.is_superset(&other.allocators) && (!other.unrestricted || self.unrestricted)
    }

    fn join(&self, other: &Self) -> Self {
        Self {
            allocators: &self.allocators | &other.allocators,
            unrestricted: self.unrestricted || other.unrestricted,
        }
    }
}

impl fmt::Debug for MemoryArea {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.allocators
                .iter()
                .map(|allocator| format!("{}", allocator.debug_fmt().short(true)))
                .chain(if self.unrestricted {
                    vec!["*".to_owned()]
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

impl Pointer {
    pub fn to(target: MemoryArea) -> Self {
        Self {
            target,
            can_be_null: false,
        }
    }

    pub fn to_null_and_(target: MemoryArea) -> Self {
        Self {
            target,
            can_be_null: true,
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
}

impl Lattice for Pointer {
    fn is_progression_of(&self, other: &Self) -> bool {
        self.target.is_progression_of(&other.target) && (!other.can_be_null || self.can_be_null)
    }

    fn join(&self, other: &Self) -> Self {
        Self {
            target: self.target.join(&other.target),
            can_be_null: self.can_be_null || other.can_be_null,
        }
    }
}

impl fmt::Debug for Pointer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?}{}",
            self.target,
            if self.can_be_null { "|null" } else { "" }
        )
    }
}

// == HeapVal ==

#[derive(Clone, PartialEq, Eq)]
pub struct Heap {
    object_infos: HashMap<Node, Rc<ObjectInfo>>,
}

impl fmt::Debug for Heap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (ptr, obj_info) in &self.object_infos {
            writeln!(f, "{} = {:?}", ptr.debug_fmt().short(true), obj_info)?
        }
        Ok(())
    }
}

impl Heap {
    pub fn start() -> Self {
        Self {
            object_infos: HashMap::new(),
        }
    }

    pub fn non_const_val(&self, ty: Ty) -> Val {
        if ty.mode().is_pointer() {
            let ty = PointerTy::from(ty).unwrap();
            let mut mem = MemoryArea::unrestricted();
            if let Some(ty) = ClassTy::from(ty.points_to()) {
                for (_, info) in &self.object_infos {
                    if info.ty == ty {
                        mem.join_mut(&info.mem);
                    }
                }
            } else {
                panic!("{:?}", ty);
            }
            Val::Pointer(Pointer::to_null_and_(mem))
        } else {
            Val::from_tarval_internal(Tarval::bad())
        }
    }

    pub fn reset_heap_accessed_by(&mut self, ptrs: MemoryArea, _nodes: HashSet<Node>) {
        /*
            if !ptrs.is_empty_ignoring_null() {
                let result = self
                    .objects
                    .iter()
                    .map(|(ptr, obj_info)| {
                        // if ptrs.contains(ptr) {
                        (*ptr, Rc::new(obj_info.resetted(&self)))
                        //} else {
                        //(*ptr, Rc::clone(obj.info))
                        //}
                    })
                    .collect();
                self.object_infos = result;
            }
        */
    }

    /*pub fn reset(&mut self) {
        let result = self
            .object_infos
            .iter()
            .map(|(ptr, obj_info)| (*ptr, Rc::new(obj_info.resetted(&self))))
            .collect();
        self.object_infos = result;
    }*/

    pub fn new_obj(&mut self, new_node: Node, obj_ty: ClassTy) -> Pointer {
        let mem = MemoryArea::single(new_node);
        let obj_info = Rc::new(ObjectInfo::new(mem.clone(), obj_ty));
        self.object_infos.insert(new_node, obj_info);
        Pointer::to(mem)
    }

    pub fn update_field(&mut self, ptr_val: Node, ptr: &Pointer, field: Entity, val: &Val) {
        let class_ty = ClassTy::from(field.owner()).unwrap();

        for (_node, intersect_obj) in self.object_infos.iter_mut() {
            if intersect_obj.ty == class_ty && intersect_obj.mem.intersects(&ptr.target) {
                let mut obj = (**intersect_obj).clone();
                obj.join_field(field, val);
                *intersect_obj = Rc::new(obj);
            }
        }

        let obj_info = self.object_infos.get(&ptr_val);
        let mut obj_info = if let Some(obj_info) = obj_info {
            (&**obj_info).clone()
        } else {
            ObjectInfo::new_non_const(class_ty, ptr.target.clone(), &self)
        };

        obj_info.update_field(field, val);
        self.object_infos.insert(ptr_val, Rc::new(obj_info));
    }

    pub fn lookup_field(&mut self, ptr_val: Node, ptr: &Pointer, field: Entity) -> Val {
        if let Some(obj_info) = self.object_infos.get(&ptr_val) {
            obj_info.lookup_field(field).clone()
        } else {
            self.non_const_val(field.ty())
        }
    }
}

impl Lattice for Heap {
    fn is_progression_of(&self, other: &Self) -> bool {
        for (p, obj_info) in &self.object_infos {
            if let Some(other_obj_info) = other.object_infos.get(&p) {
                if !obj_info.is_progression_of(other_obj_info) {
                    return false;
                }
            }
        }
        true
    }

    fn join(&self, other: &Self) -> Self {
        let mut object_infos = self.object_infos.clone();
        for (p, obj_info) in other.object_infos.iter() {
            if let Some(other_obj_info) = object_infos.get(p) {
                object_infos.insert(*p, Rc::new(obj_info.join(other_obj_info)));
            } else {
                object_infos.insert(*p, Rc::clone(&obj_info));
            }
        }
        Heap { object_infos }
    }
}

// == ObjectInfo ==

#[derive(Clone, PartialEq, Eq)]
pub struct ObjectInfo {
    ty: ClassTy,
    mem: MemoryArea,
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
        write!(f, "}} ({:?})", self.mem)
    }
}

impl ObjectInfo {
    pub fn new(mem: MemoryArea, ty: ClassTy) -> Self {
        Self {
            ty,
            mem,
            fields: ty
                .fields()
                .map(|field| {
                    let mode = field.ty().mode();
                    Val::from_tarval_initially(Tarval::zero(mode), mode)
                })
                .collect(),
        }
    }

    pub fn new_non_const(ty: ClassTy, mem: MemoryArea, heap: &Heap) -> Self {
        Self {
            ty,
            mem,
            fields: ty
                .fields()
                .map(|field| heap.non_const_val(field.ty()))
                .collect(),
        }
    }

    pub fn join_field(&mut self, field: Entity, val: &Val) {
        let field_idx = self.ty.idx_of_field(field);
        self.fields[field_idx] = val.join(&self.fields[field_idx]);
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
            mem: self.mem.join(&other.mem),
            fields: updated_fields,
        }
    }
}

/*
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

        let p1_set: MemoryArea = p1.into();
        assert_eq!(p1_set.can_be_null, false);
        assert_eq!(p1_set.pointers, [p1].iter().cloned().collect());

        let p2_set: MemoryArea = p2.into();

        let p12_set = p1_set.join(&p2_set);
        assert_eq!(p12_set.can_be_null, false);
        assert_eq!(p12_set.pointers, [p1, p2].iter().cloned().collect());

        assert!(p12_set.is_progression_of(&p1_set));

        test_lattice(&[p1_set, p2_set, p12_set]);
    }
}
*/
