#![warn(rust_2018_idioms, clippy::all)]
#![deny(clippy::correctness)]

mod emit;
mod impls;
pub mod type_expr;

pub use crate::impls::Blob;
pub use emit::{write_definition_file, Deps, EmitCtx, TypeDef};
pub use typescript_type_def_derive::TypeDef;
