use crate::type_expr::{Ident, List, TypeExpr, TypeName};

#[derive(Debug, Clone, Copy)]
pub enum TypeInfo {
    Native(NativeTypeInfo),
    Custom(CustomTypeInfo),
}

#[derive(Debug, Clone, Copy)]
pub struct NativeTypeInfo {
    pub r#ref: &'static TypeExpr,
}

#[derive(Debug, Clone, Copy)]
// TODO: better name
pub struct CustomTypeInfo {
    pub path: &'static List<Ident>,
    pub name: &'static TypeName,
    pub def: &'static TypeExpr,
}

impl TypeInfo {
    pub const fn r#ref(&self) -> &TypeExpr {
        match self {
            TypeInfo::Native(NativeTypeInfo { r#ref })
            | TypeInfo::Custom(CustomTypeInfo {
                path: _,
                name: _,
                def: r#ref,
            }) => r#ref,
        }
    }
}
