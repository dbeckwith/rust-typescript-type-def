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

// TODO: reduce usage of &'static as much as possible

/// Type information describing a "native" TypeScript type.
///
/// Native types have a definition which only uses built-in or pre-defined
/// TypeScript types. Therefore a definition for them is not emitted, and they
/// are referenced by their definition.
#[derive(Debug, Clone, Copy)]
pub struct NativeTypeInfo {
    /// A type expression describing this native type's definition.
    // TODO: rename to `expr` or something
    pub r#ref: &'static TypeExpr,
}

/// Type information describing a "defined" TypeScript type.
///
/// Defined types need to have a type definition emitted in the TypeScript
/// module. They are referenced using their name.
#[derive(Debug, Clone, Copy)]
pub struct DefinedTypeInfo {
    /// The documentation for this type definition.
    pub docs: Option<&'static Docs>,
    /// The name of this type.
    pub name: &'static TypeName,
    /// The definition of this type.
    pub def: &'static TypeExpr,
}

/// A TypeScript type expression.
///
/// This type is not intended to cover _all_ possible TypeScript type syntax,
/// only that which is needed by the types defined in this crate and as produced
/// by [`#[derive(TypeDef)]`](macro@crate::TypeDef).
#[derive(Debug, Clone, Copy)]
pub enum TypeExpr {
    /// A reference to another type's type information.
    Ref(TypeInfo),
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

/// A TypeScript type name, analogous to a Rust path with optional generics.
#[derive(Debug, Clone, Copy)]
pub struct TypeName {
    /// The documentation for this type name.
    pub docs: Option<&'static Docs>,
    /// The namespace path for this type.
    pub path: &'static List<Ident>,
    /// The name of this type.
    pub name: &'static Ident,
    /// The generic arguments for this type.
    ///
    /// If empty, the type does not have generics.
    pub generics: &'static List<TypeExpr>,
}

/// A TypeScript type-level string literal.
#[derive(Debug, Clone, Copy)]
pub struct TypeString {
    /// The documentation for this type string.
    pub docs: Option<&'static Docs>,
    /// The value of this literal.
    pub value: &'static str,
}

/// A TypeScript tuple type.
///
/// In TypeScript, tuples are represented as constant-length arrays where each
/// element can have a distinct type. Values of these types are encoded as
/// arrays in JSON, which are expected to have a constant length.
#[derive(Debug, Clone, Copy)]
pub struct Tuple {
    /// The documentation for this tuple.
    pub docs: Option<&'static Docs>,
    /// The types of the elements of this tuple.
    ///
    /// If the elements are empty, the only valid value for this type is the
    /// empty array `[]`.
    pub elements: &'static List<TypeExpr>,
}

/// A TypeScript object type.
#[derive(Debug, Clone, Copy)]
pub struct Object {
    /// The documentation for this object.
    pub docs: Option<&'static Docs>,
    /// The fields of this object.
    pub fields: &'static List<ObjectField>,
}

/// A field of a TypeScript object type.
#[derive(Debug, Clone, Copy)]
pub struct ObjectField {
    /// The documentation for this field.
    pub docs: Option<&'static Docs>,
    /// The name of this field.
    pub name: &'static TypeString,
    /// Whether this field is optional or not.
    ///
    /// This corresponds with the `?` suffix on the field name which makes it
    /// valid to omit the field entirely from the object, effectively giving it
    /// a value of `undefined`. In JSON, omitted optional fields are omitted
    /// from the object serialization.
    pub optional: bool,
    /// The type of this field.
    pub r#type: &'static TypeExpr,
}

/// A TypeScript array type.
///
/// In TypeScript, an array is distinct from a tuple by the fact that it may
/// have any length and every element has the same type (although that type may
/// be a union so the elements may have different runtime types). Values of both
/// of these types are encoded as arrays in JSON.
#[derive(Debug, Clone, Copy)]
pub struct Array {
    /// The documentation for this array.
    pub docs: Option<&'static Docs>,
    /// The type of items of this array.
    pub item: &'static TypeExpr,
}

/// A TypeScript union type.
#[derive(Debug, Clone, Copy)]
pub struct Union {
    /// The documentation for this union.
    pub docs: Option<&'static Docs>,
    /// The types that comprise this union.
    ///
    /// If the parts are empty, this type describes the vacuous union type
    /// which is equivalent to `never`. If the parts contain only one type,
    /// this type is equivalent to that type.
    // TODO: rename to members
    pub parts: &'static List<TypeExpr>,
}

/// A TypeScript intersection type.
///
/// Note that not all valid TypeScript intersection types are possible to
/// represent using JSON. In general, only object types with disjoint fields can
/// be intersected and still be accurately encoded as JSON (the resulting type
/// being an object with the combined fields of all the intersection members).
#[derive(Debug, Clone, Copy)]
pub struct Intersection {
    /// The documentation for this intersection.
    pub docs: Option<&'static Docs>,
    /// The types that comprise this intersection.
    ///
    /// If the parts are empty, this type describes the vacuous intersection
    /// type which is equivalent to `any`. If the parts contain only one
    /// type, this type is equivalent to that type.
    // TODO: rename to members
    pub parts: &'static List<TypeExpr>,
}

/// A TypeScript identifier.
///
/// Note that TypeScript's rules for valid identifiers are not checked by this
/// library. It is the user's responsibility to ensure that all identifiers are
/// valid in TypeScript in order for the resulting TypeScript module to be
/// valid.
#[derive(Debug, Clone, Copy)]
pub struct Ident(pub &'static str);

/// A documentation string.
///
/// The string value should be the plain unformatted documentation without any
/// `/**` or indentation in it. Lines may be separate by newlines.
#[derive(Debug, Clone, Copy)]
pub struct Docs(pub &'static str);

/// An alias for lists used in type expressions.
pub type List<T> = [&'static T];

impl TypeExpr {
    /// A helper function to create a type expression representing just an
    /// identifier.
    pub const fn ident(ident: &'static Ident) -> Self {
        Self::Name(TypeName::ident(ident))
    }
}

impl TypeName {
    /// A helper function to create a type name representing just an identifier.
    pub const fn ident(ident: &'static Ident) -> Self {
        Self {
            docs: None,
            path: &[],
            name: ident,
            generics: &[],
        }
    }
}
