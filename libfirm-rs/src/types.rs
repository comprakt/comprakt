use super::{Entity, Mode};
use libfirm_rs_bindings as bindings;
use std::{
    ffi::{CStr, CString},
    fmt,
};

macro_rules! gen_e {
    ($name:ident; $($var:ident($ty:ident)),*) => {
        #[derive(Clone, Copy)]
        pub enum $name {
            $(
                $var($ty),
            )*
        }

        impl TyTrait for $name {
            fn ir_type(self) -> *mut bindings::ir_type {
                match self {
                    $(
                        $name::$var(x) => x.ir_type(),
                    )*
                }
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(
                        $name::$var(x) => write!(f, "{:?}", x),
                    )*
                }
            }
        }

        $(
            #[derive(Clone, Copy)]
            pub struct $ty(*mut bindings::ir_type);
            impl $ty {
                pub fn from(ty: $name) -> Option<Self> {
                    match ty {
                        $name::$var(x) => Some(x),
                        _ => None,
                    }
                }
            }
            impl TyTrait for $ty {
                fn ir_type(self) -> *mut bindings::ir_type { self.0 }
            }
            impl From<$ty> for $name {
                fn from(item: $ty) -> $name { $name::$var(item) }
            }
        )*
    }
}

gen_e!(Ty;
    Primitive(PrimitiveTy),
    Method(MethodTy),
    Array(ArrayTy),
    Class(ClassTy),
    Segment(SegmentTy),
    Pointer(PointerTy),
    Struct(StructTy),
    Union(UnionTy),
    Other(OtherTy)
);

macro_rules! gen_debug {
    ($name:ident; $($var:ident($ty:ident)),*) => {
        $(
            impl fmt::Debug for $ty {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(
                        f,
                        stringify!($var),
                    )
                }
            }
        )*
    }
}

gen_debug!(Ty;
    Method(MethodTy),
    Segment(SegmentTy),
    Struct(StructTy),
    Union(UnionTy),
    Other(OtherTy)
);

impl Ty {
    pub fn from_ir_type(ty: *mut bindings::ir_type) -> Ty {
        unsafe {
            if bindings::is_Primitive_type(ty) != 0 {
                Ty::Primitive(PrimitiveTy(ty))
            } else if bindings::is_Method_type(ty) != 0 {
                Ty::Method(MethodTy(ty))
            } else if bindings::is_Array_type(ty) != 0 {
                Ty::Array(ArrayTy(ty))
            } else if bindings::is_Class_type(ty) != 0 {
                Ty::Class(ClassTy(ty))
            } else if bindings::is_segment_type(ty) != 0 {
                Ty::Segment(SegmentTy(ty))
            } else if bindings::is_Pointer_type(ty) != 0 {
                Ty::Pointer(PointerTy(ty))
            } else if bindings::is_Struct_type(ty) != 0 {
                Ty::Struct(StructTy(ty))
            } else if bindings::is_Union_type(ty) != 0 {
                Ty::Union(UnionTy(ty))
            } else {
                // this is either:
                // bindings::is_unknown_type(ty),
                // bindings::is_code_type(ty),
                // or unknown type kind
                Ty::Other(OtherTy(ty))
            }
        }
    }
}

impl Eq for Ty {}
impl PartialEq for Ty {
    fn eq(&self, other: &Self) -> bool {
        use self::Ty::*;
        match (self, other) {
            (Class(ty1), Class(ty2)) => ty1 == ty2,
            (Primitive(ty1), Primitive(ty2)) => ty1 == ty2,
            (Array(ty1), Array(ty2)) => ty1 == ty2,
            (Pointer(ty1), Pointer(ty2)) => ty1 == ty2,
            (Method(ty1), Method(ty2)) => panic!(
                "Comparing {:?} with {:?} is currently not supported.",
                ty1, ty2
            ),
            (Segment(ty1), Segment(ty2)) => panic!(
                "Comparing {:?} with {:?} is currently not supported.",
                ty1, ty2
            ),
            (Struct(ty1), Struct(ty2)) => panic!(
                "Comparing {:?} with {:?} is currently not supported.",
                ty1, ty2
            ),
            (Union(ty1), Union(ty2)) => panic!(
                "Comparing {:?} with {:?} is currently not supported.",
                ty1, ty2
            ),
            (Other(ty1), Other(ty2)) => panic!(
                "Comparing {:?} with {:?} is currently not supported.",
                ty1, ty2
            ),
            _ => false,
        }
    }
}

pub trait TyTrait: Sized {
    fn ir_type(self) -> *mut bindings::ir_type;

    fn pointer(self) -> PointerTy {
        PointerTy::from(Ty::from_ir_type(unsafe {
            bindings::new_type_pointer(self.ir_type())
        }))
        .expect("must return pointer type")
    }

    fn array(self) -> ArrayTy {
        ArrayTy::from(Ty::from_ir_type(unsafe {
            bindings::new_type_array(self.ir_type(), 0)
        }))
        .expect("must return array type")
    }

    fn size(self) -> u32 {
        unsafe { bindings::get_type_size(self.ir_type()) }
    }

    fn alignment(self) -> u32 {
        unsafe { bindings::get_type_alignment(self.ir_type()) }
    }

    fn mode(self) -> Mode {
        Mode::from_libfirm(unsafe { bindings::get_type_mode(self.ir_type()) })
    }
}

impl PointerTy {
    pub fn points_to(self) -> Ty {
        Ty::from_ir_type(unsafe { bindings::get_pointer_points_to_type(self.ir_type()) })
    }
}

impl Eq for PointerTy {}
impl PartialEq for PointerTy {
    fn eq(&self, other: &Self) -> bool {
        self.points_to() == other.points_to()
    }
}

impl fmt::Debug for PointerTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@{:?}", self.points_to())
    }
}

impl ArrayTy {
    pub fn variable_length(element_type: Ty) -> Ty {
        Ty::from_ir_type(unsafe { bindings::new_type_array(element_type.ir_type(), 0) })
    }
    pub fn fixed_length(element_type: Ty, length: usize) -> Ty {
        Ty::from_ir_type(unsafe { bindings::new_type_array(element_type.ir_type(), length as u32) })
    }

    pub fn element_type(self) -> Ty {
        Ty::from_ir_type(unsafe { bindings::get_array_element_type(self.ir_type()) })
    }
}

impl Eq for ArrayTy {}
impl PartialEq for ArrayTy {
    fn eq(&self, other: &Self) -> bool {
        self.element_type() == other.element_type()
    }
}

impl fmt::Debug for ArrayTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}[]", self.element_type())
    }
}

impl PrimitiveTy {
    fn from_ir_type(ir_type: *mut bindings::ir_type) -> PrimitiveTy {
        // if we trust libfirm, we could just return `PrimitiveTy(ir_type)` here
        PrimitiveTy::from(Ty::from_ir_type(ir_type)).expect("ir_type must a primitive type")
    }

    pub fn from_mode(mode: Mode) -> PrimitiveTy {
        Self::from_ir_type(unsafe { bindings::new_type_primitive(mode.libfirm_mode()) })
    }
    pub fn i32() -> PrimitiveTy {
        Self::from_ir_type(unsafe { bindings::new_type_primitive(bindings::mode::Is) })
    }
    /// Not part of MiniJava, but useful for malloc RT-function
    pub fn u32() -> PrimitiveTy {
        Self::from_ir_type(unsafe { bindings::new_type_primitive(bindings::mode::Iu) })
    }
    pub fn bool() -> PrimitiveTy {
        Self::from_ir_type(unsafe { bindings::new_type_primitive(bindings::mode::Bu) })
    }
    pub fn ptr() -> PrimitiveTy {
        Self::from_ir_type(unsafe { bindings::new_type_primitive(bindings::mode::P) })
    }
}

impl fmt::Debug for PrimitiveTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Primitive:{:?}", self.mode())
    }
}

impl Eq for PrimitiveTy {}
impl PartialEq for PrimitiveTy {
    fn eq(&self, other: &Self) -> bool {
        self.mode() == other.mode()
    }
}

impl ClassTy {
    pub fn new(name: &str) -> ClassTy {
        ClassTy::from(Ty::from_ir_type(unsafe {
            let name_c = CString::new(name).unwrap();
            let name_id = bindings::new_id_from_str(name_c.as_ptr());
            bindings::new_type_class(name_id)
        }))
        .expect("Expected class type")
    }

    pub fn default_layout(self) {
        unsafe {
            bindings::default_layout_compound_type(self.0);
        }
    }

    pub fn fields(self) -> impl Iterator<Item = Entity> {
        MemberIterator::new(self.ir_type()).filter(|entity| MethodTy::from(entity.ty()).is_none())
    }

    pub fn idx_of_field(self, field: Entity) -> usize {
        unsafe { bindings::get_class_member_index(self.ir_type(), field.ir_entity()) }
    }

    pub fn name(self) -> &'static CStr {
        // or get_compound_ident
        unsafe { CStr::from_ptr(bindings::get_compound_name(self.ir_type())) }
    }

    pub fn name_string(self) -> String {
        self.name().to_string_lossy().into_owned()
    }
}

impl Eq for ClassTy {}
impl PartialEq for ClassTy {
    fn eq(&self, other: &Self) -> bool {
        self.ir_type() == other.ir_type()
    }
}

impl fmt::Debug for ClassTy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name_string())
    }
}

impl MethodTy {
    pub fn res_count(self) -> usize {
        unsafe { bindings::get_method_n_ress(self.0) }
    }
    pub fn single_result_ty(self) -> Option<Ty> {
        if self.res_count() == 0 {
            None
        } else {
            unsafe { Some(Ty::from_ir_type(bindings::get_method_res_type(self.0, 0))) }
        }
    }
}

generate_iterator!(
    MemberIterator,
    *mut bindings::ir_type,
    get_class_n_members,
    ty,
    idx,
    usize,
    unsafe { Entity::new(bindings::get_class_member(ty, idx)) },
    Entity,
);

/// Builder for new_type_method
#[derive(Default)]
pub struct MethodTyBuilder {
    params: Vec<Ty>,
    result: Option<Ty>,
}

impl MethodTyBuilder {
    pub fn new() -> Self {
        MethodTyBuilder {
            params: Vec::new(),
            result: None,
        }
    }
    /// If the function isn't the main function don't forget to add a this
    /// param as the first param.
    pub fn add_param(&mut self, ty: Ty) {
        self.params.push(ty);
    }
    pub fn set_res(&mut self, res: Ty) {
        self.result = Some(res);
    }

    pub fn build_this_call(self) -> MethodTy {
        self.build(true)
    }

    pub fn build_no_this_call(self) -> MethodTy {
        self.build(false)
    }

    pub fn build(self, is_this_call: bool) -> MethodTy {
        let ir_type = unsafe {
            bindings::new_type_method(
                self.params.len(),
                if self.result.is_some() { 1 } else { 0 },
                false.into(), // variadic
                if is_this_call {
                    bindings::calling_convention::ThisCall
                } else {
                    bindings::cc_cdecl_set
                },
                bindings::mtp_additional_properties::NoProperty,
            )
        };
        for (i, param) in self.params.into_iter().enumerate() {
            unsafe { bindings::set_method_param_type(ir_type, i, param.ir_type()) };
        }
        if let Some(res) = self.result {
            unsafe { bindings::set_method_res_type(ir_type, 0, res.ir_type()) };
        }
        MethodTy(ir_type)
    }
}
