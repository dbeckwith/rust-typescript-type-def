use crate::{
    emit::TypeDef,
    type_expr::{
        DefinedTypeInfo, Ident, NativeTypeInfo, ObjectField, TypeArray,
        TypeDefinition, TypeExpr, TypeInfo, TypeName, TypeObject, TypeString,
        TypeTuple, TypeUnion,
    },
};

macro_rules! impl_native {
    ($ty:ty, $ts_ty:literal) => {
        impl TypeDef for $ty {
            const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
                r#ref: TypeExpr::ident(Ident($ts_ty)),
            });
        }
    };
}

/// A Rust equivalent to the JavaScript
/// [`Blob`](https://developer.mozilla.org/en-US/docs/Web/API/Blob) object.
#[derive(Debug, Clone)]
pub struct Blob(pub Vec<u8>);

impl_native!(Blob, "Blob");
impl_native!(bool, "boolean");
impl_native!(String, "string");
impl_native!(str, "string");
impl_native!(std::path::PathBuf, "string");
impl_native!(std::path::Path, "string");
impl_native!(std::ffi::CString, "string");
impl_native!(std::ffi::CStr, "string");
impl_native!(std::ffi::OsString, "string");
impl_native!(std::ffi::OsStr, "string");
#[cfg(feature = "json_value")]
impl_native!(serde_json::Number, "number");
impl_native!(std::net::IpAddr, "string");

macro_rules! impl_number {
    ($ty:ty, $name:ident) => {
        impl TypeDef for $ty {
            const INFO: TypeInfo = TypeInfo::Defined(DefinedTypeInfo {
                def: TypeDefinition {
                    docs: None,
                    path: &[],
                    name: Ident(stringify!($name)),
                    generic_vars: &[],
                    def: TypeExpr::ident(Ident("number")),
                },
                generic_args: &[],
            });
        }
    };
}

impl_number!(u8, U8);
impl_number!(u16, U16);
impl_number!(u32, U32);
impl_number!(u64, U64);
impl_number!(usize, Usize);
impl_number!(i8, I8);
impl_number!(i16, I16);
impl_number!(i32, I32);
impl_number!(i64, I64);
impl_number!(isize, Isize);
impl_number!(std::num::NonZeroU8, NonZeroU8);
impl_number!(std::num::NonZeroU16, NonZeroU16);
impl_number!(std::num::NonZeroU32, NonZeroU32);
impl_number!(std::num::NonZeroU64, NonZeroU64);
impl_number!(std::num::NonZeroUsize, NonZeroUsize);
impl_number!(std::num::NonZeroI8, NonZeroI8);
impl_number!(std::num::NonZeroI16, NonZeroI16);
impl_number!(std::num::NonZeroI32, NonZeroI32);
impl_number!(std::num::NonZeroI64, NonZeroI64);
impl_number!(std::num::NonZeroIsize, NonZeroIsize);
impl_number!(f32, F32);
impl_number!(f64, F64);

impl TypeDef for () {
    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: TypeExpr::ident(Ident("null")),
    });
}

macro_rules! impl_tuple {
    ($($var:ident),+) => {
        impl<$($var),+> TypeDef for ($($var,)+)
        where
            $($var: TypeDef,)+
        {
            const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
                r#ref: TypeExpr::Tuple(TypeTuple {
                    docs: None,
                    elements: &[$(TypeExpr::Ref(&$var::INFO),)+],
                }),
            });
        }
    };
}

impl_tuple!(T1);
impl_tuple!(T1, T2);
impl_tuple!(T1, T2, T3);
impl_tuple!(T1, T2, T3, T4);
impl_tuple!(T1, T2, T3, T4, T5);
impl_tuple!(T1, T2, T3, T4, T5, T6);
impl_tuple!(T1, T2, T3, T4, T5, T6, T7);
impl_tuple!(T1, T2, T3, T4, T5, T6, T7, T8);
impl_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9);
impl_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10);
impl_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11);
impl_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);
impl_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13);
impl_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14);
impl_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15);
impl_tuple!(
    T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16
);

impl<T, const N: usize> TypeDef for [T; N]
where
    T: TypeDef,
{
    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: TypeExpr::Tuple(TypeTuple {
            docs: None,
            elements: &[TypeExpr::Ref(&T::INFO); N],
        }),
    });
}

impl<T> TypeDef for Option<T>
where
    T: TypeDef,
{
    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: TypeExpr::Union(TypeUnion {
            docs: None,
            members: &[TypeExpr::Ref(&T::INFO), TypeExpr::ident(Ident("null"))],
        }),
    });
}

macro_rules! list_type_info {
    ($item:ty) => {
        TypeInfo::Native(NativeTypeInfo {
            r#ref: TypeExpr::Array(TypeArray {
                docs: None,
                item: &TypeExpr::Ref(&<$item>::INFO),
            }),
        })
    };
}

impl<T> TypeDef for Vec<T>
where
    T: TypeDef,
{
    const INFO: TypeInfo = list_type_info!(T);
}

impl<T> TypeDef for [T]
where
    T: TypeDef,
{
    const INFO: TypeInfo = list_type_info!(T);
}

macro_rules! set_type_info {
    ($item:ty) => {
        TypeInfo::Native(NativeTypeInfo {
            r#ref: TypeExpr::Array(TypeArray {
                docs: None,
                item: &TypeExpr::Ref(&<$item>::INFO),
            }),
        })
    };
}

impl<T, S> TypeDef for std::collections::HashSet<T, S>
where
    T: TypeDef,
    S: 'static,
{
    const INFO: TypeInfo = set_type_info!(T);
}

impl<T> TypeDef for std::collections::BTreeSet<T>
where
    T: TypeDef,
{
    const INFO: TypeInfo = set_type_info!(T);
}

macro_rules! map_type_info {
    ($key:ty, $value:ty) => {
        TypeInfo::Native(NativeTypeInfo {
            r#ref: TypeExpr::Name(TypeName {
                path: &[],
                name: Ident("Record"),
                generic_args: &[
                    TypeExpr::Ref(&<$key>::INFO),
                    TypeExpr::Ref(&<$value>::INFO),
                ],
            }),
        })
    };
}

impl<K, V, S> TypeDef for std::collections::HashMap<K, V, S>
where
    K: TypeDef,
    V: TypeDef,
    S: 'static,
{
    const INFO: TypeInfo = map_type_info!(K, V);
}

impl<K, V> TypeDef for std::collections::BTreeMap<K, V>
where
    K: TypeDef,
    V: TypeDef,
{
    const INFO: TypeInfo = map_type_info!(K, V);
}

#[cfg(feature = "json_value")]
impl<K, V> TypeDef for serde_json::Map<K, V>
where
    K: TypeDef,
    V: TypeDef,
{
    const INFO: TypeInfo = map_type_info!(K, V);
}

impl<T> TypeDef for &'static T
where
    T: TypeDef + ?Sized,
{
    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: TypeExpr::Ref(&T::INFO),
    });
}

impl<T> TypeDef for Box<T>
where
    T: TypeDef + ?Sized,
{
    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: TypeExpr::Ref(&T::INFO),
    });
}

impl<T> TypeDef for std::borrow::Cow<'static, T>
where
    T: Clone + TypeDef + ?Sized,
{
    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: TypeExpr::Ref(&T::INFO),
    });
}

impl<T> TypeDef for std::marker::PhantomData<T>
where
    T: TypeDef + ?Sized,
{
    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: TypeExpr::Ref(&T::INFO),
    });
}

impl<T, E> TypeDef for std::result::Result<T, E>
where
    T: TypeDef,
    E: TypeDef,
{
    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: TypeExpr::Union(TypeUnion {
            docs: None,
            members: &[
                TypeExpr::Object(TypeObject {
                    docs: None,
                    index_signature: None,
                    fields: &[ObjectField {
                        docs: None,
                        name: TypeString {
                            docs: None,
                            value: "Ok",
                        },
                        optional: false,
                        r#type: TypeExpr::Ref(&T::INFO),
                    }],
                }),
                TypeExpr::Object(TypeObject {
                    docs: None,
                    index_signature: None,
                    fields: &[ObjectField {
                        docs: None,
                        name: TypeString {
                            docs: None,
                            value: "Err",
                        },
                        optional: false,
                        r#type: TypeExpr::Ref(&E::INFO),
                    }],
                }),
            ],
        }),
    });
}

#[cfg(feature = "json_value")]
impl TypeDef for serde_json::Value {
    const INFO: TypeInfo = TypeInfo::Defined(DefinedTypeInfo {
        def: TypeDefinition {
            docs: None,
            path: &[],
            name: Ident("JSONValue"),
            generic_vars: &[],
            def: TypeExpr::Union(TypeUnion {
                docs: None,
                members: &[
                    TypeExpr::ident(Ident("null")),
                    TypeExpr::ident(Ident("boolean")),
                    TypeExpr::ident(Ident("number")),
                    TypeExpr::ident(Ident("string")),
                    TypeExpr::Array(TypeArray {
                        docs: None,
                        item: &TypeExpr::ident(Ident("JSONValue")),
                    }),
                    TypeExpr::Object(TypeObject {
                        docs: None,
                        index_signature: Some(
                            crate::type_expr::IndexSignature {
                                docs: None,
                                name: Ident("key"),
                                value: &TypeExpr::ident(Ident("JSONValue")),
                            },
                        ),
                        fields: &[],
                    }),
                ],
            }),
        },
        generic_args: &[],
    });
}
