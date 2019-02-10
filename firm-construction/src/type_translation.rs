use crate::type_checking::type_system::CheckedType;
use libfirm_rs::Mode;

pub fn get_firm_mode(ty: &CheckedType<'_>) -> Option<Mode> {
    match ty {
        CheckedType::Int => Some(Mode::Is()),
        CheckedType::Boolean => Some(Mode::Bu()),
        CheckedType::TypeRef(_) | CheckedType::Array(_) | CheckedType::Null => Some(Mode::P()),
        CheckedType::Void | CheckedType::UnknownType(_) => None,
    }
}
