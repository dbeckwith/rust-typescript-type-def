#[derive(Debug, Clone, Copy)]
pub enum TypeInfo {
    Native(NativeTypeInfo),
    Defined(DefinedTypeInfo),
}

#[derive(Debug, Clone, Copy)]
pub struct NativeTypeInfo {
    pub r#ref: &'static TypeExpr,
}

#[derive(Debug, Clone, Copy)]
pub struct DefinedTypeInfo {
    pub docs: Option<&'static Docs>,
    pub name: &'static TypeName,
    pub def: &'static TypeExpr,
}

#[derive(Debug, Clone, Copy)]
pub enum TypeExpr {
    Ref(TypeInfo),
    TypeName(TypeName),
    String(TypeString),
    Tuple(Tuple),
    Object(Object),
    Array(Array),
    Union(Union),
    Intersection(Intersection),
}

#[derive(Debug, Clone, Copy)]
pub struct TypeName {
    pub docs: Option<&'static Docs>,
    pub path: &'static List<Ident>,
    pub name: &'static Ident,
    pub generics: &'static List<TypeExpr>,
}

#[derive(Debug, Clone, Copy)]
pub struct TypeString {
    pub docs: Option<&'static Docs>,
    pub value: &'static str,
}

#[derive(Debug, Clone, Copy)]
pub struct Tuple {
    pub docs: Option<&'static Docs>,
    pub elements: &'static List<TypeExpr>,
}

#[derive(Debug, Clone, Copy)]
pub struct Object {
    pub docs: Option<&'static Docs>,
    pub fields: &'static List<ObjectField>,
}

#[derive(Debug, Clone, Copy)]
pub struct ObjectField {
    pub docs: Option<&'static Docs>,
    pub name: &'static TypeString,
    pub optional: bool,
    pub r#type: &'static TypeExpr,
}

#[derive(Debug, Clone, Copy)]
pub struct Array {
    pub docs: Option<&'static Docs>,
    pub item: &'static TypeExpr,
}

#[derive(Debug, Clone, Copy)]
pub struct Union {
    pub docs: Option<&'static Docs>,
    pub parts: &'static List<TypeExpr>,
}

#[derive(Debug, Clone, Copy)]
pub struct Intersection {
    pub docs: Option<&'static Docs>,
    pub parts: &'static List<TypeExpr>,
}

#[derive(Debug, Clone, Copy)]
pub struct Ident(pub &'static str);

#[derive(Debug, Clone, Copy)]
pub struct Docs(pub &'static str);

pub type List<T> = [&'static T];

impl TypeExpr {
    pub const fn ident(ident: &'static Ident) -> Self {
        Self::TypeName(TypeName::ident(ident))
    }
}

impl TypeName {
    pub const fn ident(ident: &'static Ident) -> Self {
        Self {
            docs: None,
            path: &[],
            name: ident,
            generics: &[],
        }
    }
}
