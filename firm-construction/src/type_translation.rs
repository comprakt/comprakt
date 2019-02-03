use super::FirmProgram;
use crate::type_checking::type_system::{CheckedType, TypeSystem};
use libfirm_rs::{
    types::{PrimitiveTy, Ty, TyTrait},
    Mode,
};

/// `None` indicates that the given type is not convertible, which
/// is not necessarily an error (e.g. `void`)
pub fn ty_from_checked_type<'src, 'ast>(
    ct: &CheckedType<'src>,
    type_system: &'_ TypeSystem<'src, 'ast>,
    program: &'_ FirmProgram<'src, 'ast>,
) -> Option<Ty> {
    let ty = match ct {
        CheckedType::Int => PrimitiveTy::i32().into(),
        CheckedType::Void => return None,
        CheckedType::TypeRef(class_def_id) => {
            let def = type_system.class(*class_def_id);
            let class = program.class(def).unwrap();
            let ty = class.borrow().entity.ty();
            ty.pointer().into()
            // If, for some unforeseen reason, the line above does not work,
            // return this instead: `PrimitiveTy::ptr().into()`.
            // However, this looses the class type we are pointing at.
            // We need this information in optimizations.
        }
        CheckedType::Array(checked_type) => {
            ty_from_checked_type(checked_type, type_system, program)
                .expect("Arrays are never of type `void`")
                .array()
                .pointer()
                .into()
        }
        CheckedType::Boolean => PrimitiveTy::bool().into(),
        CheckedType::Null => unreachable!(),
        CheckedType::UnknownType(_) => unreachable!(),
    };
    Some(ty)
}

pub fn get_firm_mode(ty: &CheckedType<'_>) -> Option<Mode> {
    match ty {
        CheckedType::Int => Some(Mode::Is()),
        CheckedType::Boolean => Some(Mode::Bu()),
        CheckedType::TypeRef(_) | CheckedType::Array(_) | CheckedType::Null => Some(Mode::P()),
        CheckedType::Void | CheckedType::UnknownType(_) => None,
    }
}
