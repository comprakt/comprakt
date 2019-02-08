use super::*;
use crate::firm::program_generator::Spans;
use libfirm_rs::{
    nodes::{Node, NodeDebug, ProjKind},
    types::{ClassTy, PointerTy, Ty, TyTrait},
    Entity,
};
use std::{
    collections::{HashMap, HashSet},
    fmt,
    hash::Hash,
    rc::Rc,
};

// == HeapVal ==

#[derive(Debug, Copy, Clone)]
pub enum Idx {
    Dynamic(Node),
    Const(usize, Node),
}

#[derive(Clone, PartialEq, Eq)]
pub struct Heap {
    pub object_infos: HashMap<Node, Rc<ObjectInfo>>,
    pub array_infos: HashMap<Node, Rc<ArrayInfo>>,
}

impl fmt::Debug for Heap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (&ptr, info) in &self.object_infos {
            writeln!(
                f,
                "obj {}{} = {:?}",
                ptr.debug_fmt().short(true),
                Spans::span_str(ptr),
                info
            )?
        }
        for (&ptr, info) in &self.array_infos {
            writeln!(
                f,
                "arr {}{} = {:?}",
                ptr.debug_fmt().short(true),
                Spans::span_str(ptr),
                info
            )?
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum InfoKind {
    Allocator,
    Cache,
}

impl InfoKind {
    pub fn is_allocator(self) -> bool {
        match self {
            Allocator => true,
            _ => false,
        }
    }
}

use self::InfoKind::*;

impl Heap {
    /* some test a external_args: &[Node] */
    pub fn start() -> Self {
        // TODO
        Self {
            object_infos: HashMap::new(),
            array_infos: HashMap::new(),
        }
    }

    // TODO remove
    pub fn non_const_val(&self, ty: Ty) -> NodeValue {
        if ty.mode().is_pointer() {
            let ty = PointerTy::from(ty).unwrap_or_else(|| panic!("{:?} to be pointer type", ty));
            let mut mem = MemoryArea::external();

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
            NodeValue::value(Pointer::to_null_and(mem).into())
        } else {
            NodeValue::value(Tarval::bad().into())
        }
    }

    pub fn mem_reachable_from(&self, mem: MemoryArea) -> MemoryArea {
        let mut mem = mem;
        let mut changed = true;
        while changed {
            changed = false;
            for info in self.object_infos.values() {
                if info.mem.intersects(&mem) {
                    changed |= mem.join_mut(&info.mem_pointed_to_by_fields());
                }
            }
            for info in self.array_infos.values() {
                if info.mem.intersects(&mem) {
                    changed |= mem.join_mut(&info.mem_pointed_to_by_fields());
                }
            }
        }
        mem
    }

    pub fn reset_heap_accessed_by(&self, mem: MemoryArea, _nodes: HashSet<Node>) -> Heap {
        if mem.is_empty() {
            self.clone()
        } else {
            let mem = self.mem_reachable_from(mem);

            let object_infos = self
                .object_infos
                .iter()
                .filter_map(|(ptr, info)| {
                    if info.mem.intersects(&mem) {
                        match info.kind {
                            Allocator => {
                                Some((*ptr, Rc::new(self.join_heap_obj(info.ty, &info.mem))))
                            }
                            Cache => None,
                        }
                    } else {
                        Some((*ptr, Rc::clone(info)))
                    }
                })
                .collect();

            let array_infos = self
                .array_infos
                .iter()
                .filter_map(|(ptr, info)| {
                    if info.mem.intersects(&mem) {
                        match info.kind {
                            Allocator => {
                                Some((*ptr, Rc::new(self.join_heap_arr(info.item_ty, &info.mem))))
                            }
                            Cache => None,
                        }
                    } else {
                        Some((*ptr, Rc::clone(info)))
                    }
                })
                .collect();

            Heap {
                array_infos,
                object_infos,
            }
        }
    }

    pub fn check_ptr_node(&self, node: Node) {
        match node {
            Node::Proj(_, ProjKind::Load_Res(..))
            | Node::Proj(_, ProjKind::Start_TArgs_Arg(..))
            | Node::Proj(_, ProjKind::Call_TResult_Arg(..))
            | Node::Phi(_) => {}
            node => panic!("Unexpected node {:?}", node),
        }
    }

    pub fn new_obj(&mut self, new_node: Node, obj_ty: ClassTy) -> Pointer {
        let info = Rc::new(ObjectInfo::allocator(new_node, obj_ty));
        let mem = info.mem.clone();
        self.object_infos.insert(new_node, info);
        Pointer::to(mem)
    }

    pub fn new_arr(&mut self, new_node: Node, item_ty: Ty) -> Pointer {
        let info = Rc::new(ArrayInfo::allocator(new_node, item_ty));
        let mem = info.mem.clone();
        self.array_infos.insert(new_node, info);
        Pointer::to(mem)
    }

    pub fn update_field(&mut self, ptr_node: Node, ptr: &Pointer, field: Entity, val: &NodeValue) {
        self.check_ptr_node(ptr_node);
        let class_ty = ClassTy::from(field.owner()).unwrap();

        for (_node, intersecting) in self.object_infos.iter_mut() {
            if intersecting.ty == class_ty && intersecting.mem.intersects(&ptr.target) {
                let mut item = (**intersecting).clone();
                // join, as we don't know whether `ptr_node` points to `intersecting`.
                item.join_field(field, val);
                *intersecting = Rc::new(item);
            }
        }

        self.enhance_field(ptr_node, ptr, field, val);
    }

    pub fn update_cell(
        &mut self,
        ptr_node: Node,
        ptr: &Pointer,
        idx: Idx,
        val: &NodeValue,
        item_ty: Ty,
    ) {
        self.check_ptr_node(ptr_node);

        for (_node, intersecting) in self.array_infos.iter_mut() {
            if intersecting.item_ty == item_ty && intersecting.mem.intersects(&ptr.target) {
                let mut item = (**intersecting).clone();
                // join, as we don't know whether `ptr_node` points to `intersecting`.
                item.join_cell(idx, val);
                *intersecting = Rc::new(item);
            }
        }

        self.enhance_cell(ptr_node, ptr, idx, val, item_ty);
    }

    pub fn enhance_field(&mut self, ptr_node: Node, ptr: &Pointer, field: Entity, val: &NodeValue) {
        self.check_ptr_node(ptr_node);
        let class_ty = ClassTy::from(field.owner()).unwrap();

        let info = self.object_infos.get(&ptr_node);
        let mut info = if let Some(info) = info {
            (&**info).clone()
        } else {
            self.join_heap_obj(class_ty, &ptr.target)
        };

        info.update_field(field, val);
        self.object_infos.insert(ptr_node, Rc::new(info));
    }

    pub fn enhance_cell(
        &mut self,
        ptr_node: Node,
        ptr: &Pointer,
        idx: Idx,
        val: &NodeValue,
        item_ty: Ty,
    ) {
        self.check_ptr_node(ptr_node);
        let info = self.array_infos.get(&ptr_node);
        let mut info = if let Some(info) = info {
            (&**info).clone()
        } else {
            self.join_heap_arr(item_ty, &ptr.target)
        };

        info.update_cell(idx, val.clone());
        self.array_infos.insert(ptr_node, Rc::new(info));
    }

    pub fn join_heap_obj(&self, class_ty: ClassTy, mem: &MemoryArea) -> ObjectInfo {
        // TODO add allocators of external
        // types!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        let mut result = None;
        for info in self.object_infos.values() {
            // We need to check for type as there is `external` memory where objects with
            // different class types can live.
            // Allocators are enough since caches always also write to their allocators.
            if info.kind.is_allocator() && info.mem.intersects(mem) && info.ty == class_ty {
                result = match result {
                    None => Some((**info).clone()),
                    Some(last) => Some(last.join(info, Cache)),
                };
            }
        }
        result.unwrap_or_else(|| panic!("Allocator of {:?} to exist", mem))
    }

    pub fn join_heap_arr(&self, item_ty: Ty, mem: &MemoryArea) -> ArrayInfo {
        let mut result = None;
        for info in self.array_infos.values() {
            if info.kind.is_allocator() && info.mem.intersects(mem) && info.item_ty == item_ty {
                result = match result {
                    None => Some((**info).clone()),
                    Some(last) => Some(last.join(info, Cache)),
                };
            }
        }
        result.unwrap_or_else(|| panic!("Allocator of {:?} to exist", mem))
    }

    pub fn lookup_field(&mut self, ptr_node: Node, ptr: &Pointer, field: Entity) -> NodeValue {
        self.check_ptr_node(ptr_node);
        if ptr.is_null_or_empty() {
            // this crashes anyways.
            log::error!("Found null deref");
            return NodeValue::zero(field.ty().mode());
        }

        let class_ty = ClassTy::from(field.owner()).unwrap();

        if let Some(info) = self.object_infos.get(&ptr_node) {
            info.lookup_field(field).clone()
        } else {
            // todo remove
            if ptr.target.is_external() {
                return self.non_const_val(field.ty());
            }

            let mut val: Option<NodeValue> = None;
            for (_node, intersecting) in self.object_infos.iter_mut() {
                // it is sufficient to only check allocators
                // as any chached info items always invalidate the corresponding allocators.
                if intersecting.kind.is_allocator()
                    && intersecting.ty == class_ty
                    && intersecting.mem.intersects(&ptr.target)
                {
                    val = match val {
                        Some(val) => Some(val.join(&intersecting.lookup_field(field))),
                        None => Some(intersecting.lookup_field(field).clone()),
                    };
                }
            }

            val.unwrap_or_else(|| panic!("Allocator of {:?} to exist", ptr))
        }
    }

    pub fn lookup_cell(
        &mut self,
        ptr_node: Node,
        ptr: &Pointer,
        idx: Idx,
        item_ty: Ty,
    ) -> NodeValue {
        self.check_ptr_node(ptr_node);
        if ptr.is_null_or_empty() {
            log::error!("Found null deref");
            return NodeValue::zero(item_ty.mode());
        }

        if let Some(info) = self.array_infos.get(&ptr_node) {
            info.lookup_cell(idx).clone()
        } else {
            // todo remove
            if ptr.target.is_external() {
                return self.non_const_val(item_ty);
            }

            let mut val: Option<NodeValue> = None;
            for (_node, intersecting) in self.array_infos.iter_mut() {
                if intersecting.kind.is_allocator()
                    && intersecting.item_ty == item_ty
                    && intersecting.mem.intersects(&ptr.target)
                {
                    val = match val {
                        Some(val) => Some(val.join(&intersecting.lookup_cell(idx))),
                        None => Some(intersecting.lookup_cell(idx)),
                    };
                }
            }

            val.unwrap_or_else(|| panic!("Allocator of {:?} to exist", ptr))
        }
    }
}

enum IntersectionType<'v, V> {
    Both(&'v V, &'v V),
    Only1(&'v V),
    Only2(&'v V),
}

fn intersect<'v, K, V, F>(map1: &'v HashMap<K, V>, map2: &'v HashMap<K, V>, mut f: F)
where
    K: Eq + Clone + Hash,
    F: FnMut(&'v K, IntersectionType<'v, V>),
{
    for (key, val1) in map1 {
        if let Some(val2) = map2.get(key) {
            f(key, IntersectionType::Both(val1, val2));
        } else {
            f(key, IntersectionType::Only1(val1));
        }
    }

    for (key, val2) in map2 {
        if map1.get(key).is_none() {
            f(key, IntersectionType::Only2(val2));
        }
    }
}

impl Lattice for Heap {
    fn is_progression_of(&self, _other: &Self) -> bool {
        /*for (p, info) in &self.object_infos {
            if let Some(other_info) = other.object_infos.get(&p) {
                if !info.is_progression_of(other_info) {
                    return false;
                }
            }
        }*/
        true
    }

    fn join(&self, other: &Self) -> Self {
        use self::IntersectionType::*;

        let mut object_infos: HashMap<Node, Rc<ObjectInfo>> = HashMap::new();
        intersect(
            &self.object_infos,
            &other.object_infos,
            |ptr_source, kind| match kind {
                Both(info1, info2) => {
                    object_infos.insert(
                        *ptr_source,
                        if info1 == info2 {
                            Rc::clone(info1)
                        } else {
                            assert_eq!(info1.kind, info2.kind);
                            Rc::new(info1.join(info2, info1.kind))
                        },
                    );
                }
                Only1(info) | Only2(info) => {
                    if info.kind.is_allocator() {
                        object_infos.insert(*ptr_source, Rc::clone(info));
                    }
                }
            },
        );

        let mut array_infos: HashMap<Node, Rc<ArrayInfo>> = HashMap::new();
        intersect(
            &self.array_infos,
            &other.array_infos,
            |ptr_source, kind| match kind {
                Both(info1, info2) => {
                    array_infos.insert(
                        *ptr_source,
                        if info1 == info2 {
                            Rc::clone(info1)
                        } else {
                            assert_eq!(info1.kind, info2.kind);
                            Rc::new(info1.join(info2, info1.kind))
                        },
                    );
                }
                Only1(info) | Only2(info) => {
                    if info.kind.is_allocator() {
                        array_infos.insert(*ptr_source, Rc::clone(info));
                    }
                }
            },
        );

        Heap {
            object_infos,
            array_infos,
        }
    }
}

/*
trait ItemInfo {
    fn mem(&self) -> MemoryArea;
    fn mem_pointed_to_by_fields(&self) -> MemoryArea;
    fn mem_reachable_from(&self) -> MemoryArea;
    fn resetted(&self, mem: &MemoryArea) -> Self;
}*/

// == ArrayInfo ==

#[derive(Clone, PartialEq, Eq)]
struct CellInfo {
    idx_source: Option<Node>,
    val: NodeValue,
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
    fn new(idx_source: Option<Node>, val: NodeValue) -> Self {
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

    pub fn join_val(&self, other: &NodeValue) -> Self {
        CellInfo {
            idx_source: None,
            val: self.val.join(other),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
enum ArrayInfoState {
    Const(HashMap<usize, CellInfo>),
    Dynamic(Node, NodeValue),
}

#[derive(Clone, PartialEq, Eq)]
pub struct ArrayInfo {
    kind: InfoKind,
    item_ty: Ty,
    mem: MemoryArea,
    default_val: NodeValue,
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
    pub fn allocator(allocator: Node, item_ty: Ty) -> Self {
        let mode = item_ty.mode();
        let mem = MemoryArea::single(allocator);
        Self {
            kind: Allocator,
            item_ty,
            mem,
            default_val: NodeValue::zero(mode),
            state: ArrayInfoState::Const(HashMap::new()),
        }
    }

    /*
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
    */

    pub fn join_cell(&mut self, idx: Idx, val: &NodeValue) {
        let existing_val = self.lookup_cell(idx);
        self.update_cell(idx, existing_val.join(val));
    }

    fn any_item(&self) -> NodeValue {
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

    pub fn update_cell(&mut self, idx: Idx, val: NodeValue) {
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

    pub fn lookup_cell(&self, idx: Idx) -> NodeValue {
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

    fn join(&self, other: &Self, kind: InfoKind) -> Self {
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
            kind,
        }
    }
}

/*
impl Lattice for ArrayInfo {
    fn is_progression_of(&self, other: &Self) -> bool {
        assert!(self.item_ty == other.item_ty);

        / *
        TODO
        for (idx, val) in self.vals.iter().enumerate() {
            if !field_val.is_progression_of(&other.fields[field_idx]) {
                return false;
            }
        }* /
true
}

}
*/

// == ObjectInfo ==

#[derive(Clone, PartialEq, Eq)]
pub struct ObjectInfo {
    kind: InfoKind,
    ty: ClassTy,
    mem: MemoryArea,
    // stores a value for each field
    fields: Vec<NodeValue>,
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
    pub fn allocator(allocator: Node, ty: ClassTy) -> Self {
        let mem = MemoryArea::single(allocator);
        Self {
            kind: Allocator,
            ty,
            mem,
            fields: ty
                .fields()
                .map(|field| NodeValue::zero(field.ty().mode()))
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

    /*
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
    */

    pub fn join_field(&mut self, field: Entity, val: &NodeValue) {
        let field_idx = self.ty.idx_of_field(field);
        self.fields[field_idx] = val.join(&self.fields[field_idx]);
    }

    pub fn update_field(&mut self, field: Entity, val: &NodeValue) {
        let field_idx = self.ty.idx_of_field(field);
        self.fields[field_idx] = val.clone();
    }

    pub fn lookup_field(&self, field: Entity) -> &NodeValue {
        let idx = self.ty.idx_of_field(field);
        &self.fields[idx]
    }

    fn join(&self, other: &Self, kind: InfoKind) -> Self {
        assert!(self.ty == other.ty);

        let mut updated_fields = Vec::with_capacity(self.fields.len());
        for (field_idx, field_val) in self.fields.iter().enumerate() {
            updated_fields.push(field_val.join(&other.fields[field_idx]));
        }
        Self {
            ty: self.ty,
            mem: self.mem.join(&other.mem),
            fields: updated_fields,
            kind,
        }
    }
}
/*
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
*/
