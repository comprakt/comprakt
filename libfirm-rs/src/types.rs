use libfirm_rs_bindings as bindings;
use std::ffi::{CString};

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
                    $($name::$var(x) => x.ir_type(),)*
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
            } else if bindings::is_unknown_type(ty) != 0
                    || bindings::is_code_type(ty) != 0 {
                Ty::Other(OtherTy(ty))
            } else {
                Ty::Other(OtherTy(ty))
                // unknown type kind found
            }
        }
    }
}

trait TyTrait : Sized {
    fn ir_type(self) -> *mut bindings::ir_type;

    fn pointer(self) -> PointerTy {
        PointerTy::from(
            Ty::from_ir_type(
                unsafe { bindings::new_type_pointer(self.ir_type()) }
            )
        ).expect("must return pointer type")
    }

    fn array(self) -> ArrayTy {
        ArrayTy::from(
            Ty::from_ir_type(
                unsafe { bindings::new_type_array(self.ir_type(), 0) }.into()
            )
        ).expect("must return array type")
    }

    fn size(self) -> u32 {
        unsafe { bindings::get_type_size(self.ir_type()) }
    }

    fn alignment(self) -> u32 {
        unsafe { bindings::get_type_alignment(self.ir_type()) }
    }

    fn mode(self) -> bindings::mode::Type {
        unsafe { bindings::get_type_mode(self.ir_type()) }
    }
}

impl PointerTy {
    pub fn points_to(self) -> Ty {
        Ty::from_ir_type(
            unsafe { bindings::get_pointer_points_to_type(self.ir_type()) }
        )
    }
}

impl ArrayTy {
    pub fn variable_length(element_type: Ty) -> Ty {
        Ty::from_ir_type(
            unsafe { bindings::new_type_array(element_type.ir_type(), 0) }
        )
    }
    pub fn fixed_length(element_type: Ty, length: usize) -> Ty {
        Ty::from_ir_type(
            unsafe { bindings::new_type_array(element_type.ir_type(), length as u32) }
        )
    }

    pub fn element_type(self) -> Ty {
        Ty::from_ir_type(
            unsafe { bindings::get_array_element_type(self.ir_type()) }
        )
    }
}

impl PrimitiveTy {
    fn from_ir_type(ir_type: *mut bindings::ir_type) -> PrimitiveTy {
        // if we trust libfirm, we could just return `PrimitiveTy(ir_type)` here
        PrimitiveTy::from(
            Ty::from_ir_type(ir_type)
        ).expect("ir_type must a primitive type")
    }

    pub fn from_mode(mode: *mut bindings::ir_mode) -> PrimitiveTy {
        Self::from_ir_type(unsafe { bindings::new_type_primitive(mode) })
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
        Self::from_ir_type(unsafe { bindings::new_type_primitive(bindings::mode::P) }.into())
    }
}

impl ClassTy {
    pub fn new_class_type(name: &str) -> ClassTy {
        ClassTy::from(
            Ty::from_ir_type(
                unsafe {
                    bindings::new_type_class(
                        CString::new(name)
                        .expect("CString::new failed").as_ptr() as *mut _
                    )
                }
            )
        ).expect("Expected class type")
    }
}

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
