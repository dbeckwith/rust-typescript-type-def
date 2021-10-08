# Changelog

## v0.3.1

* Add `#[type_def(type_of = "T")]` attribute to better support using foreign types, see docs for usage

## v0.3.0

* Breaking changes to `TypeInfo` to properly support generics

  Support for defining type information for generic Rust types was added in v0.2.2, but the implementation was incorrect. Generic Rust types would have non-generic TypeScript definitions emitted for each specific instance of the type used, all under the same name. Now, exactly one generic type definition is emitted. In order to implement this properly, the `TypeInfo` type has changed slightly to support generic types. If you've been using the `TypeDef` derive macro, the changes should not affect you. If you need to implement `TypeDef` for generic types manually, please refer to the documentation of the `DefinedTypeInfo` struct for important implementation requirements.

## v0.2.3

* Add changelog
* Document serde attribute support
* Support `#[serde(default = "default_fn")]`

## v0.2.2

* Support generic types in derive macro

## v0.2.1

* Support `#[serde(skip)]` on fields and variants in derive macro
* Support `#[serde(rename)]` on types, fields, and variants in derive macro

## v0.2.0

* Some changes to `TypeInfo` structure
* Internal performance improvements

## v0.1.1

* Support multi-line custom header for generated file

## v0.1.0

Initial release
