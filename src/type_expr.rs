//! This module defines structs used to create static descriptions of TypeScript
//! type definitions.

use crate::{
    emit::{Emit, EmitCtx},
    DefinitionFileOptions,
};
use std::io;

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

impl TypeInfo {
    /// Emit the type name with generics as an expression.
    pub fn emit_expr<'ctx>(
        &'static self,
        w: &'ctx mut dyn io::Write,
        options: &'ctx DefinitionFileOptions<'ctx>,
    ) -> io::Result<()> {
        let expr = TypeExpr::Ref(self);
        let mut ctx = EmitCtx::new(w, options.clone());
        expr.emit(&mut ctx)?;
        Ok(())
    }
}

/// Type information describing a "native" TypeScript type.
///
/// Native types have a definition which only uses built-in or pre-defined
/// TypeScript types. Therefore a definition for them is not emitted, and they
/// are referenced by their definition.
#[derive(Debug, Clone, Copy)]
pub struct NativeTypeInfo {
    /// A type expression describing this native type.
    pub r#ref: TypeExpr,
}

/// Type information describing a "defined" TypeScript type.
///
/// Defined types need to have a type definition emitted in the TypeScript
/// module. They are referenced using their name.
#[derive(Debug, Clone, Copy)]
pub struct DefinedTypeInfo {
    /// The definition of this type.
    ///
    /// # Implementation Note
    /// The body of the definition **must** be *invariant* of the Rust
    /// type's generic parameters. In other words, if the Rust type is generic,
    /// its definition must be the same for any value of its generic
    /// parameters. Where the definition needs to reference generic parameters,
    /// you must instead use a placeholder type whose
    /// [`INFO`](crate::emit::TypeDef::INFO) is [`TypeInfo::Native`] and the
    /// native type reference is a [`TypeExpr::Name`] referencing the generic
    /// parameter.
    pub def: TypeDefinition,
    /// The specific values of the generic arguments of this type for this
    /// instance of the generic type.
    ///
    /// This list should contain references to the type info of each type
    /// parameter of this Rust type. Unlike `def`, these values **should**
    /// depend on the generic parameters of this type.
    pub generic_args: List<TypeExpr>,
}

/// The TypeScript definition of a type.
#[derive(Debug, Clone, Copy)]
pub struct TypeDefinition {
    /// The documentation for this type definition.
    pub docs: Option<Docs>,
    /// The namespace path for this type.
    pub path: List<Ident>,
    /// The name of this type.
    pub name: Ident,
    /// The generic variables for this type defintion.
    ///
    /// If empty, the type does not have generics.
    pub generic_vars: List<Ident>,
    /// The definition of this type.
    pub def: TypeExpr,
}

/// A TypeScript type expression.
///
/// This type is not intended to cover _all_ possible TypeScript type syntax,
/// only that which is needed by the types defined in this crate and as produced
/// by [`#[derive(TypeDef)]`](macro@crate::TypeDef).
#[derive(Debug, Clone, Copy)]
pub enum TypeExpr {
    /// A reference to another type.
    Ref(&'static TypeInfo),
    /// A reference to a bare type name which should already be defined.
    Name(TypeName),
    /// A type-level string literal.
    String(TypeString),
    /// A tuple type.
    Tuple(TypeTuple),
    /// An object type.
    Object(TypeObject),
    /// An array type.
    Array(TypeArray),
    /// A union type.
    Union(TypeUnion),
    /// An intersection type.
    Intersection(TypeIntersection),
}

/// A reference to a built-in TypeScript type, analogous to a Rust path with
/// optional generics.
#[derive(Debug, Clone, Copy)]
pub struct TypeName {
    /// The namespace path for this type.
    pub path: List<Ident>,
    /// The name of this type.
    pub name: Ident,
    /// The generic arguments for this type.
    ///
    /// If empty, the type does not have generics.
    pub generic_args: List<TypeExpr>,
}

/// A TypeScript type-level string literal.
#[derive(Debug, Clone, Copy)]
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
#[derive(Debug, Clone, Copy)]
pub struct TypeTuple {
    /// The documentation for this tuple.
    pub docs: Option<Docs>,
    /// The types of the elements of this tuple.
    ///
    /// If the elements are empty, the only valid value for this type is the
    /// empty array `[]`.
    pub elements: List<TypeExpr>,
}

/// A TypeScript object type.
#[derive(Debug, Clone, Copy)]
pub struct TypeObject {
    /// The documentation for this object.
    pub docs: Option<Docs>,
    /// The fields of this object.
    pub fields: List<ObjectField>,
}

/// A field of a TypeScript object type.
#[derive(Debug, Clone, Copy)]
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
#[derive(Debug, Clone, Copy)]
pub struct TypeArray {
    /// The documentation for this array.
    pub docs: Option<Docs>,
    /// The type of items of this array.
    pub item: &'static TypeExpr,
}

/// A TypeScript union type.
#[derive(Debug, Clone, Copy)]
pub struct TypeUnion {
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
#[derive(Debug, Clone, Copy)]
pub struct TypeIntersection {
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
#[derive(Debug, Clone, Copy)]
pub struct Ident(pub &'static str);

/// A documentation string.
///
/// The string value should be the plain unformatted documentation without any
/// `/**` or indentation in it. Lines may be separate by newlines.
#[derive(Debug, Clone, Copy)]
pub struct Docs(pub &'static str);

/// An alias for lists used in type expressions.
pub type List<T> = &'static [T];

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
            generic_args: &[],
        }
    }
}
