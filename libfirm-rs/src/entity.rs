use super::{
    bindings,
    types::{Ty, TyTrait},
    Graph,
};
use std::{
    ffi::{CStr, CString},
    hash::{Hash, Hasher},
};

#[derive(Clone, Copy, From, Into)]
pub struct Entity(*mut bindings::ir_entity);

impl Entity {
    pub fn new_entity(owning_ty: Ty, name: &str, ty: Ty) -> Self {
        unsafe {
            let name_c = CString::new(name).unwrap();
            let name_id = bindings::new_id_from_str(name_c.as_ptr());
            let entity = bindings::new_entity(owning_ty.ir_type(), name_id, ty.ir_type());
            Self::new(entity)
        }
    }

    pub fn new_global(id: &str, ty: Ty) -> Self {
        Self::new(unsafe {
            let global_type = bindings::get_glob_type();
            let name_c = CString::new(id).unwrap();
            let name = bindings::new_id_from_str(name_c.as_ptr());
            bindings::new_entity(global_type, name, ty.ir_type())
        })
    }

    pub fn new(ir_entity: *mut bindings::ir_entity) -> Self {
        Self(ir_entity)
    }

    pub fn ir_entity(self) -> *mut bindings::ir_entity {
        self.0
    }

    pub fn ty(self) -> Ty {
        unsafe { Ty::from_ir_type(bindings::get_entity_type(self.0)) }
    }

    pub fn owner(self) -> Ty {
        unsafe { Ty::from_ir_type(bindings::get_entity_owner(self.0)) }
    }

    pub fn name(self) -> &'static CStr {
        unsafe { CStr::from_ptr(bindings::get_entity_name(self.0)) }
    }

    pub fn name_string(self) -> String {
        self.name().to_string_lossy().into_owned()
    }

    pub fn ld_name(self) -> &'static CStr {
        unsafe { CStr::from_ptr(bindings::get_entity_ld_name(self.0)) }
    }

    pub fn graph(self) -> Option<Graph> {
        unsafe {
            let irg = bindings::get_entity_irg(self.0);
            if irg.is_null() {
                None
            } else {
                Some(Graph { irg })
            }
        }
    }

    pub fn offset(self) -> i32 {
        unsafe { bindings::get_entity_offset(self.0) }
    }
}

impl Hash for Entity {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ir_entity().hash(state);
    }
}

impl PartialEq for Entity {
    fn eq(&self, other: &Self) -> bool {
        self.ir_entity() == other.ir_entity()
    }
}

impl Eq for Entity {}
