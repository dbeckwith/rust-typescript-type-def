use crate::{
    emit::{Deps, EmitCtx, TypeDef},
    type_expr::{
        Array,
        DefinedTypeInfo,
        Ident,
        NativeTypeInfo,
        Tuple,
        TypeExpr,
        TypeInfo,
        TypeName,
        Union,
    },
};
use std::io;

macro_rules! impl_native {
    ($ty:ty, $ts_ty:literal) => {
        impl TypeDef for $ty {
            type Deps = ();

            const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
                r#ref: &TypeExpr::ident(&Ident($ts_ty)),
            });
        }
    };
}

pub struct Blob(pub Vec<u8>);
impl_native!(Blob, "Blob");
impl_native!(bool, "boolean");
impl_native!(String, "string");
impl_native!(str, "string");

macro_rules! impl_number {
    ($ty:ty, $name:ident) => {
        impl TypeDef for $ty {
            type Deps = ();

            const INFO: TypeInfo = TypeInfo::Defined(DefinedTypeInfo {
                docs: None,
                name: &TypeName::ident(&Ident(stringify!($name))),
                def: &TypeExpr::ident(&Ident("number")),
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

impl crate::emit::private::Sealed for () {}
impl Deps for () {
    fn emit_each(_ctx: &mut EmitCtx<'_>) -> io::Result<()> {
        Ok(())
    }
}

macro_rules! impl_tuple {
    ($($var:ident),+) => {
        impl<$($var),+> crate::emit::private::Sealed for ($($var,)+) {}
        impl<$($var),+> Deps for ($($var,)+)
        where
            $($var: TypeDef,)+
        {
            fn emit_each(ctx: &mut EmitCtx<'_>) -> io::Result<()> {
                $(ctx.emit_type::<$var>()?;)+
                Ok(())
            }
        }

        impl<$($var),+> TypeDef for ($($var,)+)
        where
            $($var: TypeDef,)+
        {
            type Deps = Self;

            const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
                r#ref: &TypeExpr::Tuple(Tuple {
                    docs: None,
                    elements: &[$(&TypeExpr::Ref($var::INFO),)+],
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
    type Deps = (T,);

    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: &TypeExpr::Tuple(Tuple {
            docs: None,
            elements: &[&TypeExpr::Ref(T::INFO); N],
        }),
    });
}

impl<T> TypeDef for Option<T>
where
    T: TypeDef,
{
    type Deps = (T,);

    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: &TypeExpr::Union(Union {
            docs: None,
            parts: &[&TypeExpr::Ref(T::INFO), &TypeExpr::ident(&Ident("null"))],
        }),
    });
}

impl<T> TypeDef for Vec<T>
where
    T: TypeDef,
{
    type Deps = (T,);

    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: &TypeExpr::Array(Array {
            docs: None,
            item: &TypeExpr::Ref(T::INFO),
        }),
    });
}

impl<T> TypeDef for [T]
where
    T: TypeDef,
{
    type Deps = (T,);

    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: &TypeExpr::Array(Array {
            docs: None,
            item: &TypeExpr::Ref(T::INFO),
        }),
    });
}

macro_rules! impl_set {
    ($($ty:ident)::+) => {
        impl<T> TypeDef for $($ty)::+<T>
        where
            T: TypeDef,
        {
            type Deps = (T,);

            const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
                r#ref: &TypeExpr::Array(Array {
                    docs: None,
                    item: &TypeExpr::Ref(T::INFO),
                }),
            });
        }
    };
}

impl_set!(std::collections::HashSet);
impl_set!(std::collections::BTreeSet);

macro_rules! impl_map {
    ($($ty:ident)::+) => {
        impl<K, V> TypeDef for $($ty)::+<K, V>
        where
            K: TypeDef,
            V: TypeDef,
        {
            type Deps = (K, V);

            const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
                r#ref: &TypeExpr::Name(TypeName {
                    docs: None,
                    path: &[],
                    name: &Ident("Record"),
                    generics: &[
                        &TypeExpr::Ref(K::INFO),
                        &TypeExpr::Ref(V::INFO),
                    ],
                }),
            });
        }
    };
}

impl_map!(std::collections::HashMap);
impl_map!(std::collections::BTreeMap);

impl<T> TypeDef for &'static T
where
    T: TypeDef,
{
    type Deps = (T,);

    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: &TypeExpr::Ref(T::INFO),
    });
}

impl<T> TypeDef for Box<T>
where
    T: TypeDef,
{
    type Deps = (T,);

    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: &TypeExpr::Ref(T::INFO),
    });
}

impl<T> TypeDef for std::borrow::Cow<'static, T>
where
    T: Clone + TypeDef,
{
    type Deps = (T,);

    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: &TypeExpr::Ref(T::INFO),
    });
}
