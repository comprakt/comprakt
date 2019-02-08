use super::FirmProgram;
use crate::type_checking::type_system::{CheckedType, TypeSystem};
use libfirm_rs::{
    types::{PrimitiveTy, StructTy, Ty, TyTrait},
    Mode,
};

/// `None` indicates that the given type is not convertible, which
/// is not necessarily an error (e.g. `void`)
///
/// An `Array(T)` is represented as pointers
/// to sth like this: ```
/// struct $Array<T> {
///   len: i32,
///   data: T[],
/// }
/// ```
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
            let array_data = ty_from_checked_type(checked_type, type_system, program)
                .expect("Arrays are never of type `void`")
                .array();

            // TODO This is a shitty "generic" array, in theory we need only a unique type
            // definition inner type
            let safe_array = StructTy::new_anon("$Array");
            safe_array.new_subentity("len", PrimitiveTy::i32());
            safe_array.new_subentity("data", array_data);

            safe_array.default_layout();

            safe_array.pointer().into()
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
