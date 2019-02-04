use super::{Lattice, Val};
use crate::firm::program_generator::Spans;
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
    arbitrary: bool,
}

impl MemoryArea {
    pub fn empty() -> Self {
        Self {
            allocators: HashSet::new(),
            unrestricted: false,
            arbitrary: false,
        }
    }

    pub fn single(allocator: Node) -> Self {
        let mut allocators = HashSet::new();
        allocators.insert(allocator);
        Self {
            allocators,
            unrestricted: false,
            arbitrary: false,
        }
    }

    pub fn unrestricted() -> Self {
        Self {
            allocators: HashSet::new(),
            unrestricted: true,
            arbitrary: false,
        }
    }

    #[allow(dead_code)]
    pub fn arbitrary() -> Self {
        Self {
            allocators: HashSet::new(),
            unrestricted: true,
            arbitrary: true,
        }
    }

    pub fn intersects(&self, other: &Self) -> bool {
        self.arbitrary
            || other.arbitrary
            || (self.unrestricted && other.unrestricted)
            || !self.allocators.is_disjoint(&other.allocators)
    }

    pub fn is_empty(&self) -> bool {
        self.allocators.is_empty() && !self.unrestricted
    }

    pub fn join_mut(&mut self, other: &Self) {
        self.allocators.extend(&other.allocators);
        self.unrestricted |= other.unrestricted;
        self.arbitrary |= other.arbitrary;
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
                .chain(if self.unrestricted {
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

impl Pointer {
    pub fn to(target: MemoryArea) -> Self {
        Self {
            target,
            can_be_null: false,
        }
    }

    #[allow(clippy::wrong_self_convention)]
    pub fn to_null_and(target: MemoryArea) -> Self {
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

    pub fn is_null_or_empty(&self) -> bool {
        self.target.is_empty()
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

// == HeapVal ==

#[derive(Debug, Copy, Clone)]
pub enum Idx {
    Node(Node),
    Const(usize),
}

#[derive(Clone, PartialEq, Eq)]
pub struct Heap {
    object_infos: HashMap<Node, Rc<ObjectInfo>>,
    array_infos: HashMap<Node, Rc<ArrayInfo>>,
}

impl fmt::Debug for Heap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (&ptr, obj_info) in &self.object_infos {
            writeln!(
                f,
                "obj {}{} = {:?}",
                ptr.debug_fmt().short(true),
                Spans::span_str(ptr),
                obj_info
            )?
        }
        for (&ptr, arr_info) in &self.array_infos {
            writeln!(
                f,
                "arr {}{} = {:?}",
                ptr.debug_fmt().short(true),
                Spans::span_str(ptr),
                arr_info
            )?
        }
        Ok(())
    }
}

impl Heap {
    pub fn start() -> Self {
        Self {
            object_infos: HashMap::new(),
            array_infos: HashMap::new(),
        }
    }

    pub fn non_const_val(&self, ty: Ty) -> Val {
        if ty.mode().is_pointer() {
            let ty = PointerTy::from(ty).unwrap_or_else(|| panic!("{:?} to be pointer type", ty));
            let mut mem = MemoryArea::unrestricted();

            match ty.points_to() {
                Ty::Class(ty) => {
                    for info in self.object_infos.values() {
                        if info.ty == ty {
                            mem.join_mut(&info.mem);
                        }
                    }
                }
                Ty::Array(ty) => {
                    let item_ty = ty.element_type();
                    for info in self.array_infos.values() {
                        if info.item_ty == item_ty {
                            mem.join_mut(&info.mem);
                        }
                    }
                }
                other_ty => panic!("unexpected type {:?}", other_ty),
            }
            Val::Pointer(Pointer::to_null_and(mem))
        } else {
            Val::from_tarval_internal(Tarval::bad())
        }
    }

    pub fn reset_heap_accessed_by(&mut self, mem: MemoryArea, _nodes: HashSet<Node>) {
        // for now, reset everything
        if !mem.is_empty() {
            self.reset();
        }
    }

    pub fn reset(&mut self) {
        self.object_infos = self
            .object_infos
            .iter()
            .map(|(ptr, obj_info)| (*ptr, Rc::new(obj_info.resetted(&self))))
            .collect();

        self.array_infos = self
            .array_infos
            .iter()
            .map(|(ptr, arr_info)| (*ptr, Rc::new(arr_info.resetted(&self))))
            .collect();
    }

    pub fn new_obj(&mut self, new_node: Node, obj_ty: ClassTy) -> Pointer {
        let mem = MemoryArea::single(new_node);
        let obj_info = Rc::new(ObjectInfo::new(mem.clone(), obj_ty));
        self.object_infos.insert(new_node, obj_info);
        Pointer::to(mem)
    }

    pub fn update_field(&mut self, ptr_node: Node, ptr: &Pointer, field: Entity, val: &Val) {
        let class_ty = ClassTy::from(field.owner()).unwrap();

        for (_node, intersect_obj) in self.object_infos.iter_mut() {
            if intersect_obj.ty == class_ty && intersect_obj.mem.intersects(&ptr.target) {
                let mut obj = (**intersect_obj).clone();
                // downgrade information as we don't know whether ptr_node points to obj.
                obj.join_field(field, val);
                *intersect_obj = Rc::new(obj);
            }
        }

        let obj_info = self.object_infos.get(&ptr_node);
        let mut obj_info = if let Some(obj_info) = obj_info {
            (&**obj_info).clone()
        } else {
            // we could be more precise here
            ObjectInfo::new_non_const(class_ty, ptr.target.clone(), &self)
        };

        obj_info.update_field(field, val);
        self.object_infos.insert(ptr_node, Rc::new(obj_info));
    }

    pub fn lookup_field(&mut self, ptr_node: Node, ptr: &Pointer, field: Entity) -> Val {
        if ptr.is_null_or_empty() {
            return Val::zero(field.ty().mode());
        }

        let class_ty = ClassTy::from(field.owner()).unwrap();

        if let Some(obj_info) = self.object_infos.get(&ptr_node) {
            obj_info.lookup_field(field).clone()
        } else {
            if ptr.target.unrestricted {
                return self.non_const_val(field.ty());
            }

            let mut val: Option<Val> = None;
            for (_node, intersect_obj) in self.object_infos.iter_mut() {
                if intersect_obj.ty == class_ty && intersect_obj.mem.intersects(&ptr.target) {
                    val = match val {
                        Some(val) => Some(val.join(&intersect_obj.lookup_field(field))),
                        None => Some(intersect_obj.lookup_field(field).clone()),
                    };
                }
            }

            match val {
                Some(val) => val,
                None => {
                    // maybe implement later when we don't delete objects:
                    // assert_ne!(val, Val::NoInfoYet);
                    self.non_const_val(field.ty())
                }
            }
        }
    }

    pub fn new_arr(&mut self, new_node: Node, item_ty: Ty) -> Pointer {
        let mem = MemoryArea::single(new_node);
        let arr_info = Rc::new(ArrayInfo::new(mem.clone(), item_ty));
        self.array_infos.insert(new_node, arr_info);
        Pointer::to(mem)
    }

    pub fn update_cell(&mut self, ptr_node: Node, ptr: &Pointer, idx: Idx, val: &Val, item_ty: Ty) {
        for (_node, intersect_arr) in self.array_infos.iter_mut() {
            if intersect_arr.item_ty == item_ty && intersect_arr.mem.intersects(&ptr.target) {
                let mut arr = (**intersect_arr).clone();
                arr.join_cell(idx, val);
                *intersect_arr = Rc::new(arr);
            }
        }

        let arr_info = self.array_infos.get(&ptr_node);
        let mut arr_info = if let Some(arr_info) = arr_info {
            (&**arr_info).clone()
        } else {
            ArrayInfo::new_non_const(item_ty, ptr.target.clone(), &self)
        };

        arr_info.update_cell(idx, val.clone());
        self.array_infos.insert(ptr_node, Rc::new(arr_info));
    }

    pub fn lookup_cell(&mut self, ptr_node: Node, ptr: &Pointer, idx: Idx, item_ty: Ty) -> Val {
        if ptr.is_null_or_empty() {
            return Val::zero(item_ty.mode());
        }

        if let Some(arr_info) = self.array_infos.get(&ptr_node) {
            arr_info.lookup_cell(idx).clone()
        } else {
            if ptr.target.unrestricted {
                return self.non_const_val(item_ty);
            }

            let mut val: Option<Val> = None;
            for (_node, intersect_arr) in self.array_infos.iter_mut() {
                if intersect_arr.item_ty == item_ty && intersect_arr.mem.intersects(&ptr.target) {
                    val = match val {
                        Some(val) => Some(val.join(&intersect_arr.lookup_cell(idx))),
                        None => Some(intersect_arr.lookup_cell(idx)),
                    };
                }
            }

            match val {
                Some(val) => val,
                None => {
                    self.non_const_val(item_ty)
                    /*
                    TODO implement later when use dom depth information

                    log::error!("check why error occured:");
                    for (_node, intersect_arr) in self.array_infos.iter_mut() {
                        log::debug!(
                            "check for item ty '{:?}' with '{:?}' => {:?}",
                            item_ty,
                            intersect_arr.item_ty,
                            item_ty == intersect_arr.item_ty
                        );
                        log::debug!(
                            "check for intersection '{:?}' with '{:?}' => {:?}",
                            &ptr.target,
                            intersect_arr.mem,
                            intersect_arr.mem.intersects(&ptr.target)
                        );
                        if intersect_arr.item_ty == item_ty
                            && intersect_arr.mem.intersects(&ptr.target)
                        {
                            val = val.join(&intersect_arr.lookup_cell(idx));
                        }
                    }
                    panic!("see log");
                    */
                }
            }
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
        let mut object_infos: HashMap<Node, Rc<ObjectInfo>> = HashMap::new();
        for (p, obj_info) in other.object_infos.iter() {
            if let Some(other_obj_info) = object_infos.get(p) {
                object_infos.insert(*p, Rc::new(obj_info.join(other_obj_info)));
            } else {
                let joined = Rc::new(obj_info.resetted(&self));
                object_infos.insert(*p, joined);
            }
        }

        for (p, obj_info) in self.object_infos.iter() {
            if !object_infos.contains_key(p) {
                let joined = Rc::new(obj_info.resetted(&self));
                object_infos.insert(*p, joined);
            }
        }

        let mut array_infos: HashMap<Node, Rc<ArrayInfo>> = HashMap::new();
        for (p, arr_info) in other.array_infos.iter() {
            if let Some(other_arr_info) = array_infos.get(p) {
                array_infos.insert(*p, Rc::new(arr_info.join(other_arr_info)));
            } else {
                let joined = Rc::new(arr_info.resetted(&self));
                array_infos.insert(*p, joined);
            }
        }

        for (p, arr_info) in self.array_infos.iter() {
            if !array_infos.contains_key(p) {
                let joined = Rc::new(arr_info.resetted(&self));
                array_infos.insert(*p, joined);
            }
        }

        Heap {
            object_infos,
            array_infos,
        }
    }
}

// == ArrayInfo ==

#[derive(Clone, PartialEq, Eq, Debug)]
enum ArrayInfoState {
    Const(HashMap<usize, Val>),
    Dynamic(Node, Val),
}

#[derive(Clone, PartialEq, Eq)]
pub struct ArrayInfo {
    item_ty: Ty,
    mem: MemoryArea,
    default_val: Val,
    state: ArrayInfoState,
    //size: usize,
}

impl fmt::Debug for ArrayInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}[] {{", self.item_ty)?;
        let mut first = true;
        match &self.state {
            ArrayInfoState::Const(cells) => {
                for (idx, val) in cells {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {:?}", idx, val)?;
                    first = false;
                }
            }
            ArrayInfoState::Dynamic(node, val) => {
                write!(f, "[{}]: {:?}", node.debug_fmt().short(true), val)?;
                first = false;
            }
        }

        if !first {
            write!(f, ", ")?;
        }
        write!(f, "{:?}", self.default_val)?;
        write!(f, "}} ({:?})", self.mem)
    }
}

impl ArrayInfo {
    pub fn new(mem: MemoryArea, item_ty: Ty) -> Self {
        let mode = item_ty.mode();
        Self {
            item_ty,
            mem,
            default_val: Val::zero(mode),
            state: ArrayInfoState::Const(HashMap::new()),
        }
    }

    pub fn resetted(&self, heap: &Heap) -> Self {
        Self::new_non_const(self.item_ty, self.mem.clone(), heap)
    }

    pub fn new_non_const(item_ty: Ty, mem: MemoryArea, heap: &Heap) -> Self {
        Self {
            item_ty,
            mem,
            default_val: heap.non_const_val(item_ty),
            state: ArrayInfoState::Const(HashMap::new()),
        }
    }

    pub fn join_cell(&mut self, idx: Idx, val: &Val) {
        let existing_val = self.lookup_cell(idx);
        self.update_cell(idx, existing_val.join(val));
    }

    fn any_item(&self) -> Val {
        let mut val = self.default_val.clone();
        match &self.state {
            ArrayInfoState::Const(cells) => {
                for cell_val in cells.values() {
                    val = val.join(cell_val);
                }
            }
            ArrayInfoState::Dynamic(_, cell_val) => {
                val = val.join(cell_val);
            }
        }
        val
    }

    pub fn update_cell(&mut self, idx: Idx, val: Val) {
        match (idx, &mut self.state) {
            (Idx::Const(idx), ArrayInfoState::Const(ref mut cells)) => {
                // [1: a, 3: b, default][1] := x => [1: x, 3: b, default]
                // [1: a, 3: b, default][2] := x => [1: a, 2: x, 3: b, default]
                cells.insert(idx, val);
            }
            (Idx::Node(node), ArrayInfoState::Const(_cells)) => {
                // [1: a, 2: b, default][i] := x => [ [node]: val, join(default, a, b, val)]
                self.default_val = self.any_item().join(&val);
                self.state = ArrayInfoState::Dynamic(node, val);
            }
            (Idx::Const(idx), ArrayInfoState::Dynamic(_node, old_val)) => {
                // [ [node]: val, default ][1] := x => [ 1: val, join(default, val)]
                let mut cells = HashMap::new();
                cells.insert(idx, val);
                self.default_val = self.default_val.join(old_val);
                self.state = ArrayInfoState::Const(cells);
            }
            (Idx::Node(node), ArrayInfoState::Dynamic(last_node, old_val)) => {
                if node == *last_node {
                    // [ [last_node]: old, default ][node] := x => [ [last_node]: val, default ]
                } else {
                    // [ [last_node]: old, def ][node] := x => [ [node]: val, join(old, def)]
                    self.default_val = self.default_val.join(old_val);
                }
                self.state = ArrayInfoState::Dynamic(node, val);
            }
        }
    }

    pub fn lookup_cell(&self, idx: Idx) -> Val {
        match (idx, &self.state) {
            (Idx::Const(idx), ArrayInfoState::Const(cells)) => {
                // [1: a, 3: b, default][1] = a
                // [1: a, 3: b, default][2] = default
                cells.get(&idx).unwrap_or(&self.default_val).clone()
            }
            (Idx::Node(_), ArrayInfoState::Const(_))
            | (Idx::Const(_), ArrayInfoState::Dynamic(_, _)) => {
                // [1: a, 2: b, default][i] = join(a, b, default)
                // [ [node]: val, default ][1] = join(val, default)
                self.any_item()
            }
            (Idx::Node(last_node), ArrayInfoState::Dynamic(node, val)) => {
                if last_node == *node {
                    // [ [last_node]: val, default ][last_node] = val
                    val.clone()
                } else {
                    // [ [last_node]: val, default ][node] = join(val, default)
                    val.join(&self.default_val)
                }
            }
        }
    }
}

impl Lattice for ArrayInfo {
    fn is_progression_of(&self, other: &Self) -> bool {
        assert!(self.item_ty == other.item_ty);

        /*
        TODO
        for (idx, val) in self.vals.iter().enumerate() {
            if !field_val.is_progression_of(&other.fields[field_idx]) {
                return false;
            }
        }*/
        true
    }

    fn join(&self, other: &Self) -> Self {
        assert!(self.item_ty == other.item_ty);

        let mut default_val = self.default_val.join(&other.default_val);

        use self::ArrayInfoState::*;
        let state = match (&self.state, &other.state) {
            (Const(cells1), Const(cells2)) => {
                let mut cells = HashMap::new();

                for (idx, val1) in cells1 {
                    if let Some(val2) = cells2.get(idx) {
                        cells.insert(*idx, val1.join(val2));
                    } else {
                        cells.insert(*idx, val1.join(&other.default_val));
                    }
                }
                for (idx, val2) in cells2 {
                    if !cells1.contains_key(&idx) {
                        cells.insert(*idx, val2.join(&self.default_val));
                    }
                }

                Const(cells)
            }
            (Dynamic(node1, val1), Dynamic(node2, val2)) => {
                if node1 == node2 {
                    Dynamic(*node1, val1.join(val2))
                } else {
                    // we could either keep node1 or node2 but not both.
                    // for symmetry reasons, we discard both.
                    default_val = default_val.join(val1).join(&val2);
                    Const(HashMap::new())
                }
            }
            (Dynamic(_, _), Const(_)) | (Const(_), Dynamic(_, _)) => {
                // [ 1: a, 3: b, default1 ] & [ [node]: c, default2 ]
                // = [ join(a, b, default1, c, default2) ]
                // we lose all information :(
                default_val = self.any_item().join(&other.any_item());
                Const(HashMap::new())
            }
        };

        Self {
            item_ty: self.item_ty,
            mem: self.mem.join(&other.mem),
            default_val,
            state,
        }
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
        write!(f, "{} {{", self.ty.name_string())?;
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
                .map(|field| Val::zero(field.ty().mode()))
                .collect(),
        }
    }

    pub fn resetted(&self, heap: &Heap) -> Self {
        Self::new_non_const(self.ty, self.mem.clone(), heap)
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
