//! This module defines structs used to create static descriptions of TypeScript
//! type definitions.

/// A description of the type information required to produce a TypeScript type
/// definition.
#[derive(Debug, Clone, Copy)]
pub enum TypeInfo {
    /// This info describes a "native" TypeScript type which does not require a
    /// type definition.
    Native(NativeTypeInfo),
    /// This info describes a "defined" TypeScript type which does require a
    /// type definition.
    Defined(DefinedTypeInfo),
}

/// Type information describing a "native" TypeScript type.
///
/// Native types have a definition which only uses built-in or pre-defined
/// TypeScript types. Therefore a definition for them is not emitted, and they
/// are referenced by their definition.
#[derive(Debug, Clone, Copy)]
pub struct NativeTypeInfo {
    /// A type expression describing this native type's definition.
    pub def: TypeExpr,
}

/// Type information describing a "defined" TypeScript type.
///
/// Defined types need to have a type definition emitted in the TypeScript
/// module. They are referenced using their name.
#[derive(Debug, Clone, Copy)]
pub struct DefinedTypeInfo {
    /// The documentation for this type definition.
    pub docs: Option<Docs>,
    /// The name of this type.
    pub name: TypeName,
    /// The definition of this type.
    pub def: TypeExpr,
}

/// A TypeScript type expression.
///
/// This type is not intended to cover _all_ possible TypeScript type syntax,
/// only that which is needed by the types defined in this crate and as produced
/// by [`#[derive(TypeDef)]`](macro@crate::TypeDef).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeExpr {
    /// A reference to another type.
    Ref(&'static TypeRef),
    /// A reference to a bare type name which should already be defined.
    Name(TypeName),
    /// A type-level string literal.
    String(TypeString),
    /// A tuple type.
    Tuple(Tuple),
    /// An object type.
    Object(Object),
    /// An array type.
    Array(Array),
    /// A union type.
    Union(Union),
    /// An intersection type.
    Intersection(Intersection),
}

/// A reference to another TypeScript type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeRef {
    /// A reference to a native type.
    ///
    /// Native types are referenced just by the expression representing the
    /// type.
    Native(TypeExpr),
    /// A reference to a defined type.
    ///
    /// Defined types are referenced by their name.
    Defined(TypeName),
}

/// A TypeScript type name, analogous to a Rust path with optional generics.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeName {
    /// The namespace path for this type.
    pub path: List<Ident>,
    /// The name of this type.
    pub name: Ident,
    /// The generic arguments for this type.
    ///
    /// If empty, the type does not have generics.
    pub generics: List<TypeExpr>,
}

/// A TypeScript type-level string literal.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
// TODO: should docs be included in hash?
// could make a custom hash key wrapper
pub struct TypeString {
    /// The documentation for this type string.
    pub docs: Option<Docs>,
    /// The value of this literal.
    pub value: &'static str,
}

/// A TypeScript tuple type.
///
/// In TypeScript, tuples are represented as constant-length arrays where each
/// element can have a distinct type. Values of these types are encoded as
/// arrays in JSON, which are expected to have a constant length.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Tuple {
    /// The documentation for this tuple.
    pub docs: Option<Docs>,
    /// The types of the elements of this tuple.
    ///
    /// If the elements are empty, the only valid value for this type is the
    /// empty array `[]`.
    pub elements: List<TypeExpr>,
}

/// A TypeScript object type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Object {
    /// The documentation for this object.
    pub docs: Option<Docs>,
    /// The fields of this object.
    pub fields: List<ObjectField>,
}

/// A field of a TypeScript object type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ObjectField {
    /// The documentation for this field.
    pub docs: Option<Docs>,
    /// The name of this field.
    pub name: TypeString,
    /// Whether this field is optional or not.
    ///
    /// This corresponds with the `?` suffix on the field name which makes it
    /// valid to omit the field entirely from the object, effectively giving it
    /// a value of `undefined`. In JSON, omitted optional fields are omitted
    /// from the object serialization.
    pub optional: bool,
    /// The type of this field.
    pub r#type: TypeExpr,
}

/// A TypeScript array type.
///
/// In TypeScript, an array is distinct from a tuple by the fact that it may
/// have any length and every element has the same type (although that type may
/// be a union so the elements may have different runtime types). Values of both
/// of these types are encoded as arrays in JSON.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Array {
    /// The documentation for this array.
    pub docs: Option<Docs>,
    /// The type of items of this array.
    pub item: &'static TypeExpr,
}

/// A TypeScript union type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Union {
    /// The documentation for this union.
    pub docs: Option<Docs>,
    /// The types that comprise this union.
    ///
    /// If the members are empty, this type describes the vacuous union type
    /// which is equivalent to `never`. If the members contain only one type,
    /// this type is equivalent to that type.
    pub members: List<TypeExpr>,
}

/// A TypeScript intersection type.
///
/// Note that not all valid TypeScript intersection types are possible to
/// represent using JSON. In general, only object types with disjoint fields can
/// be intersected and still be accurately encoded as JSON (the resulting type
/// being an object with the combined fields of all the intersection members).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Intersection {
    /// The documentation for this intersection.
    pub docs: Option<Docs>,
    /// The types that comprise this intersection.
    ///
    /// If the members are empty, this type describes the vacuous intersection
    /// type which is equivalent to `any`. If the members contain only one
    /// type, this type is equivalent to that type.
    pub members: List<TypeExpr>,
}

/// A TypeScript identifier.
///
/// Note that TypeScript's rules for valid identifiers are not checked by this
/// library. It is the user's responsibility to ensure that all identifiers are
/// valid in TypeScript in order for the resulting TypeScript module to be
/// valid.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident(pub &'static str);

/// A documentation string.
///
/// The string value should be the plain unformatted documentation without any
/// `/**` or indentation in it. Lines may be separate by newlines.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Docs(pub &'static str);

/// An alias for lists used in type expressions.
pub type List<T> = &'static [T];

impl TypeInfo {
    /// A helper function to create a reference to this type info.
    pub const fn r#ref(self) -> TypeRef {
        match self {
            TypeInfo::Native(NativeTypeInfo { def }) => TypeRef::Native(def),
            TypeInfo::Defined(DefinedTypeInfo {
                docs: _,
                name,
                def: _,
            }) => TypeRef::Defined(name),
        }
    }
}

impl TypeExpr {
    /// A helper function to create a type expression representing just an
    /// identifier.
    pub const fn ident(ident: Ident) -> Self {
        Self::Name(TypeName::ident(ident))
    }
}

impl TypeName {
    /// A helper function to create a type name representing just an identifier.
    pub const fn ident(ident: Ident) -> Self {
        Self {
            path: &[],
            name: ident,
            generics: &[],
        }
    }
}

impl TypeExpr {
    pub(crate) fn iter_refs(self) -> impl Iterator<Item = TypeRef> {
        IterRefs::new(self)
    }
}

struct IterRefs {
    exprs: Vec<TypeExpr>,
}

impl IterRefs {
    fn new(expr: TypeExpr) -> Self {
        Self { exprs: vec![expr] }
    }
}

impl Iterator for IterRefs {
    type Item = TypeRef;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.exprs.pop()? {
                TypeExpr::Ref(type_ref) => return Some(*type_ref),
                TypeExpr::Name(TypeName {
                    path: _,
                    name: _,
                    generics,
                }) => {
                    self.exprs.extend(generics);
                },
                TypeExpr::String(TypeString { docs: _, value: _ }) => {},
                TypeExpr::Tuple(Tuple { docs: _, elements }) => {
                    self.exprs.extend(elements);
                },
                TypeExpr::Object(Object { docs: _, fields }) => {
                    self.exprs.extend(fields.iter().map(
                        |ObjectField {
                             docs: _,
                             name: _,
                             optional: _,
                             r#type,
                         }| *r#type,
                    ));
                },
                TypeExpr::Array(Array { docs: _, item }) => {
                    self.exprs.push(*item);
                },
                TypeExpr::Union(Union { docs: _, members }) => {
                    self.exprs.extend(members);
                },
                TypeExpr::Intersection(Intersection { docs: _, members }) => {
                    self.exprs.extend(members);
                },
            }
        }
    }
}
