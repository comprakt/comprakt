use super::{Lattice, NodeValue, Val};
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
        self.allocators.is_empty() && !self.unrestricted && !self.arbitrary
    }

    pub fn join_mut(&mut self, other: &Self) -> bool /* changed */ {
        let old = (self.allocators.len(), self.unrestricted, self.arbitrary);
        self.allocators.extend(&other.allocators);
        self.unrestricted |= other.unrestricted;
        self.arbitrary |= other.arbitrary;
        old != (self.allocators.len(), self.unrestricted, self.arbitrary)
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
    Dynamic(Node),
    Const(usize, Node),
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
            Val::NodeValue(NodeValue::Pointer(Pointer::to_null_and(mem)), None)
        } else {
            Val::from_tarval_internal(Tarval::bad(), None)
        }
    }

    pub fn reset_heap_accessed_by(&self, mem: MemoryArea, _nodes: HashSet<Node>) -> Heap {
        if mem.is_empty() {
            self.clone()
        } else {
            let mut mem = mem;
            let mut changed = true;
            while changed {
                changed = false;
                for obj_info in self.object_infos.values() {
                    if obj_info.mem.intersects(&mem) {
                        changed |= mem.join_mut(&obj_info.mem_pointed_to_by_fields());
                    }
                }
                for arr_info in self.array_infos.values() {
                    if arr_info.mem.intersects(&mem) {
                        changed |= mem.join_mut(&arr_info.mem_pointed_to_by_fields());
                    }
                }
            }

            let object_infos = self
                .object_infos
                .iter()
                .map(|(ptr, obj_info)| {
                    if obj_info.mem.intersects(&mem) {
                        (*ptr, Rc::new(obj_info.resetted(&self)))
                    } else {
                        (*ptr, Rc::clone(obj_info))
                    }
                })
                .collect();

            let array_infos = self
                .array_infos
                .iter()
                .map(|(ptr, arr_info)| {
                    if arr_info.mem.intersects(&mem) {
                        (*ptr, Rc::new(arr_info.resetted(&self)))
                    } else {
                        (*ptr, Rc::clone(arr_info))
                    }
                })
                .collect();

            Heap {
                array_infos,
                object_infos,
            }
        }
    }

    #[allow(dead_code)]
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

        self.enhance_field(ptr_node, ptr, field, val);
    }

    pub fn enhance_field(&mut self, ptr_node: Node, ptr: &Pointer, field: Entity, val: &Val) {
        let class_ty = ClassTy::from(field.owner()).unwrap();

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
            return Val::zero(field.ty().mode(), None);
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

        self.enhance_cell(ptr_node, ptr, idx, val, item_ty);
    }

    pub fn enhance_cell(
        &mut self,
        ptr_node: Node,
        ptr: &Pointer,
        idx: Idx,
        val: &Val,
        item_ty: Ty,
    ) {
        let arr_info = self.array_infos.get(&ptr_node);
        let mut arr_info = if let Some(arr_info) = arr_info {
            (&**arr_info).clone()
        } else {
            ArrayInfo::new_non_const(item_ty, ptr.target.clone(), &self)
        };

        if let ArrayInfoState::Const(cells) = &arr_info.state {
            if let Idx::Dynamic(_idx) = idx {
                if cells.len() >= 2 {
                    // enhance would lose more information than gained
                    return;
                }
            }
        }

        arr_info.update_cell(idx, val.clone());
        self.array_infos.insert(ptr_node, Rc::new(arr_info));
    }

    pub fn lookup_cell(&mut self, ptr_node: Node, ptr: &Pointer, idx: Idx, item_ty: Ty) -> Val {
        if ptr.is_null_or_empty() {
            return Val::zero(item_ty.mode(), None);
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

        for (p, self_obj_info) in self.object_infos.iter() {
            if let Some(other_obj_info) = other.object_infos.get(p) {
                object_infos.insert(*p, Rc::new(self_obj_info.join(other_obj_info)));
            } else {
                let joined = Rc::new(self_obj_info.resetted(&self));
                object_infos.insert(*p, joined);
            }
        }

        for (p, other_obj_info) in other.object_infos.iter() {
            if !self.object_infos.contains_key(p) {
                let joined = Rc::new(other_obj_info.resetted(&self));
                object_infos.insert(*p, joined);
            }
        }

        let mut array_infos: HashMap<Node, Rc<ArrayInfo>> = HashMap::new();
        for (p, self_arr_info) in self.array_infos.iter() {
            if let Some(other_arr_info) = other.array_infos.get(p) {
                array_infos.insert(*p, Rc::new(self_arr_info.join(other_arr_info)));
            } else {
                let joined = Rc::new(self_arr_info.resetted(&self));
                array_infos.insert(*p, joined);
            }
        }

        for (p, other_arr_info) in other.array_infos.iter() {
            if !self.array_infos.contains_key(p) {
                let joined = Rc::new(other_arr_info.resetted(&self));
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

#[derive(Clone, PartialEq, Eq)]
struct CellInfo {
    idx_source: Option<Node>,
    val: Val,
}

impl fmt::Debug for CellInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{:?}",
            if let Some(node) = self.idx_source {
                format!("= [{:?}]: ", node)
            } else {
                "".to_owned()
            },
            self.val
        )
    }
}

impl CellInfo {
    fn new(idx_source: Option<Node>, val: Val) -> Self {
        Self { idx_source, val }
    }

    pub fn join(&self, other: &Self) -> Self {
        CellInfo {
            idx_source: match (self.idx_source, other.idx_source) {
                (Some(a), Some(b)) if a == b => Some(a),
                _ => None,
            },
            val: self.val.join(&other.val),
        }
    }

    pub fn join_val(&self, other: &Val) -> Self {
        CellInfo {
            idx_source: None,
            val: self.val.join(other),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
enum ArrayInfoState {
    Const(HashMap<usize, CellInfo>),
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
            default_val: Val::zero(mode, None),
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
                    val = val.join(&cell_val.val);
                }
            }
            ArrayInfoState::Dynamic(_, cell_val) => {
                val = val.join(cell_val);
            }
        }
        val
    }

    pub fn mem_pointed_to_by_fields(&self) -> MemoryArea {
        let mut mem = self.default_val.points_to().clone();
        match &self.state {
            ArrayInfoState::Const(cells) => {
                for item_val in cells.values() {
                    mem.join_mut(&item_val.val.points_to());
                }
            }
            ArrayInfoState::Dynamic(_node, val) => {
                mem.join_mut(&val.points_to());
            }
        }

        mem
    }

    pub fn update_cell(&mut self, idx: Idx, val: Val) {
        match (idx, &mut self.state) {
            (Idx::Const(idx, idx_source), ArrayInfoState::Const(ref mut cells)) => {
                // [ 1: a, 3: b, [*]: def ][1] := x => [ 1: x, 3: b, [*]: def ]
                // [ 1: a, 3: b, [*]: def ][2] := x => [ 1: a, 2: x, 3: b, [*]: def ]
                cells.insert(idx, CellInfo::new(Some(idx_source), val));
            }
            (Idx::Dynamic(node), ArrayInfoState::Const(_cells)) => {
                // [ 1: a, 2: b, [*]: def ][i] := x => [ [node]: val, [*]: def & a & b & val) ]
                self.default_val = self.any_item().join(&val);
                self.state = ArrayInfoState::Dynamic(node, val);
            }
            (Idx::Const(idx, idx_source), ArrayInfoState::Dynamic(_node, old_val)) => {
                // [ [node]: val, [*]: def ][1] := x => [ 1: val, [*]: def & val ]
                let mut cells = HashMap::new();
                cells.insert(idx, CellInfo::new(Some(idx_source), val));
                self.default_val = self.default_val.join(old_val);
                self.state = ArrayInfoState::Const(cells);
            }
            (Idx::Dynamic(node), ArrayInfoState::Dynamic(last_node, old_val)) => {
                if node == *last_node {
                    // [ [last_node]: old, [*]: def ][node] := x => [ [node]: val, [*]: def ]
                } else {
                    // [ [last_node]: old, [*]: def ][node] := x => [ [node]: val, [*]: old & def ]
                    self.default_val = self.default_val.join(old_val);
                }
                self.state = ArrayInfoState::Dynamic(node, val);
            }
        }
    }

    pub fn lookup_cell(&self, idx: Idx) -> Val {
        match (idx, &self.state) {
            (Idx::Const(idx, _), ArrayInfoState::Const(cells)) => {
                // [ 1: a, 3: b, [*]: def ][1] = a
                // [ 1: a, 3: b, [*]: def ][2] = def
                cells
                    .get(&idx)
                    .map(|v| &v.val)
                    .unwrap_or(&self.default_val)
                    .clone()
            }
            (Idx::Dynamic(idx_node), ArrayInfoState::Const(cells)) => {
                if let Some(cell) = cells.values().find(|v| v.idx_source == Some(idx_node)) {
                    // [ 1@idx_node: a, 2: b, [*]: def ][idx_node] = a
                    cell.val.clone()
                } else {
                    // [ 1: a, 2: b, [*]: def ][idx_node] = a & b & def
                    self.any_item()
                }
            }
            (Idx::Const(_idx_val, idx_node), ArrayInfoState::Dynamic(node, val)) => {
                if idx_node == *node {
                    // [ [node]: val, [*]: def ][1@idx_node] = val
                    val.clone()
                } else {
                    // [ [node]: val, [*]: def ][1] = val & def
                    // we cannot use `_idx_val` for anything useful here
                    self.any_item()
                }
            }
            (Idx::Dynamic(idx_node), ArrayInfoState::Dynamic(node, val)) => {
                if idx_node == *node {
                    // [ [node]: val, [*]: def ][idx_node] = val
                    val.clone()
                } else {
                    // [ [node]: val, [*]: def ][idx_node] = val & def
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
                        cells.insert(*idx, val1.join_val(&other.default_val));
                    }
                }
                for (idx, val2) in cells2 {
                    if !cells1.contains_key(&idx) {
                        cells.insert(*idx, val2.join_val(&self.default_val));
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
            (Dynamic(idx_source, val), Const(cells)) | (Const(cells), Dynamic(idx_source, val)) => {
                if let Some((idx, cell)) = cells
                    .iter()
                    .find(|(_idx, v)| v.idx_source == Some(*idx_source))
                {
                    // [ 1@idx_source: a, 3: b, [*]: def1 ] & [ [idx_source]: c, [*]: def2 ]
                    // = [ [node]: a & c, [*]: b & def1 & def2) ]

                    // IMPROVEMENT: `any_item` except cell!
                    default_val = self.any_item().join(&other.any_item());
                    let mut cells = HashMap::new();
                    cells.insert(*idx, CellInfo::new(Some(*idx_source), cell.val.join(val)));
                    Const(cells)
                } else {
                    // [ 1: a, 3: b, [*]: def1 ] & [ [node]: c, [*]: def2 ]
                    // = [ [*]: a & b & def1 & c & def2) ]

                    // we lose all information :(

                    default_val = self.any_item().join(&other.any_item());
                    Const(HashMap::new())
                }
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
            let field_name = name.split('$').last().unwrap();
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
                .map(|field| Val::zero(field.ty().mode(), None))
                .collect(),
        }
    }

    pub fn mem_pointed_to_by_fields(&self) -> MemoryArea {
        let mut mem = MemoryArea::empty();
        for field_val in &self.fields {
            mem.join_mut(&field_val.points_to());
        }
        mem
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
