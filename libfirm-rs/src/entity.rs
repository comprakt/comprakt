use super::Ty;
use libfirm_rs_bindings as bindings;
use std::ffi::CStr;

#[derive(Clone, Copy, From, Into)]
pub struct Entity(*mut bindings::ir_entity);

impl Entity {
    pub fn new(ir_entity: *mut bindings::ir_entity) -> Entity {
        Entity(ir_entity)
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

    pub fn ty(self) -> Ty {
        unsafe { bindings::get_entity_type(self.0) }.into()
    }
}
