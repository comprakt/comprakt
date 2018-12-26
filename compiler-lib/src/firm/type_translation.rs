use crate::type_checking::type_system::CheckedType;
use libfirm_rs::{
    bindings,
    types::{PrimitiveTy, Ty, TyTrait},
};

/// `None` indicates that the given type is not convertible, which
/// is not necessarily an error (e.g. `void`)
pub fn ty_from_checked_type(ct: &CheckedType<'_>) -> Option<Ty> {
    let ty = match ct {
        CheckedType::Int => PrimitiveTy::i32().into(),
        CheckedType::Void => return None,
        CheckedType::TypeRef(_) => PrimitiveTy::ptr().into(),
        CheckedType::Array(checked_type) => ty_from_checked_type(checked_type)
            .expect("Arrays are never of type `void`")
            .array()
            .pointer()
            .into(),
        CheckedType::Boolean => PrimitiveTy::bool().into(),
        CheckedType::Null => unreachable!(),
        CheckedType::UnknownType(_) => unreachable!(),
    };
    Some(ty)
}

pub fn get_firm_mode(ty: &CheckedType<'_>) -> Option<bindings::mode::Type> {
    match ty {
        CheckedType::Int => Some(unsafe { bindings::mode::Is }),
        CheckedType::Boolean => Some(unsafe { bindings::mode::Bu }),
        CheckedType::TypeRef(_) | CheckedType::Array(_) | CheckedType::Null => {
            Some(unsafe { bindings::mode::P })
        }
        CheckedType::Void | CheckedType::UnknownType(_) => None,
    }
}

pub fn size_of(ty: &CheckedType<'_>) -> Option<u32> {
    get_firm_mode(ty).map(|mode| unsafe { bindings::get_mode_size_bytes(mode) })
}
