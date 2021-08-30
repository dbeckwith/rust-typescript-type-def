# Changelog

## Unreleased

* Remove `Blob` type
* Breaking changes to `TypeInfo` to properly support generics. Generics support was added in v0.2.2, but the implementation was incorrect.
<!-- TODO: elaborate changes -->

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
