#![warn(rust_2018_idioms, clippy::all)]
#![deny(clippy::correctness)]

mod emit;
mod impls;
pub mod type_expr;

pub use crate::{
    emit::{write_definition_file, Deps, Stats, TypeDef},
    impls::Blob,
};

/// A derive proc-macro for the [`TypeDef`] trait.
///
/// This macro can be used on `struct`s and `enum`s which also derive
/// [`serde::Serialize`](https://docs.rs/serde/latest/serde/trait.Serialize.html)
/// and/or
/// [`serde::Deserialize`](https://docs.rs/serde/latest/serde/trait.Deserialize.html),
/// and will generate a [`TypeDef`] implementation which matches the shape
/// of the JSON produced by using [`serde_json`](https://docs.rs/serde_json/) on
/// the target type. This macro will also read and adapt to `#[serde(...)]`
/// attributes on the target type's definition.
///
/// This macro also reads the following attributes:
/// * `#[type_def(namespace = "x.y.z")]` on the `struct`/`enum` body puts
///   the TypeScript type definition under a namespace of `x.y.z`.
// TODO: examples
pub use typescript_type_def_derive::TypeDef;
