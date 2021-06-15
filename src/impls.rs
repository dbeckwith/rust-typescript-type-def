use crate::{
    emit::{Deps, EmitCtx, TypeDef},
    type_expr::{Array, Ident, Tuple, TypeExpr, TypeName, Union},
    type_info::{CustomTypeInfo, NativeTypeInfo, TypeInfo},
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

macro_rules! impl_alias {
    ($ty:ty, $name:ident, $ts_ty:literal) => {
        impl TypeDef for $ty {
            type Deps = ();

            const INFO: TypeInfo = TypeInfo::Custom(CustomTypeInfo {
                path: &[],
                name: &TypeName::ident(&Ident(stringify!($name))),
                def: &TypeExpr::ident(&Ident(stringify!($ts_ty))),
            });
        }
    };
}

impl_alias!(u8, U8, "number");
impl_alias!(u16, U16, "number");
impl_alias!(u32, U32, "number");
impl_alias!(u64, U64, "number");
impl_alias!(usize, Usize, "number");
impl_alias!(i8, I8, "number");
impl_alias!(i16, I16, "number");
impl_alias!(i32, I32, "number");
impl_alias!(i64, I64, "number");
impl_alias!(isize, Isize, "number");
impl_alias!(std::num::NonZeroU8, NonZeroU8, "number");
impl_alias!(std::num::NonZeroU16, NonZeroU16, "number");
impl_alias!(std::num::NonZeroU32, NonZeroU32, "number");
impl_alias!(std::num::NonZeroU64, NonZeroU64, "number");
impl_alias!(std::num::NonZeroUsize, NonZeroUsize, "number");
impl_alias!(std::num::NonZeroI8, NonZeroI8, "number");
impl_alias!(std::num::NonZeroI16, NonZeroI16, "number");
impl_alias!(std::num::NonZeroI32, NonZeroI32, "number");
impl_alias!(std::num::NonZeroI64, NonZeroI64, "number");
impl_alias!(std::num::NonZeroIsize, NonZeroIsize, "number");
impl_alias!(f32, F32, "number");
impl_alias!(f64, F64, "number");

impl Deps for () {
    fn emit_each(_ctx: &mut EmitCtx<'_>) -> io::Result<()> {
        Ok(())
    }
}

macro_rules! impl_tuple {
    ($($var:ident),+) => {
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
                r#ref: &TypeExpr::Tuple(Tuple(&[
                    $($var::INFO.r#ref(),)+
                ])),
            });
        }
    };
}

impl_tuple!(T0);
impl_tuple!(T0, T1);
impl_tuple!(T0, T1, T2);
impl_tuple!(T0, T1, T2, T3);
impl_tuple!(T0, T1, T2, T3, T4);
impl_tuple!(T0, T1, T2, T3, T4, T5);
impl_tuple!(T0, T1, T2, T3, T4, T5, T6);
impl_tuple!(T0, T1, T2, T3, T4, T5, T6, T7);

impl<T, const N: usize> TypeDef for [T; N]
where
    T: TypeDef,
{
    type Deps = (T,);

    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: &TypeExpr::Tuple(Tuple(&[T::INFO.r#ref(); N])),
    });
}

impl<T> TypeDef for Option<T>
where
    T: TypeDef,
{
    type Deps = (T,);

    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: &TypeExpr::Union(Union(&[
            T::INFO.r#ref(),
            &TypeExpr::ident(&Ident("null")),
        ])),
    });
}

impl<T> TypeDef for Vec<T>
where
    T: TypeDef,
{
    type Deps = (T,);

    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: &TypeExpr::Array(Array(T::INFO.r#ref())),
    });
}

impl<T> TypeDef for [T]
where
    T: TypeDef,
{
    type Deps = (T,);

    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: &TypeExpr::Array(Array(T::INFO.r#ref())),
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
                r#ref: &TypeExpr::Array(Array(T::INFO.r#ref())),
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
                r#ref: &TypeExpr::TypeName(TypeName {
                    name: &Ident("Record"),
                    generics: &[K::INFO.r#ref(), V::INFO.r#ref()],
                }),
            });
        }
    };
}

impl_map!(std::collections::HashMap);
impl_map!(std::collections::BTreeMap);

impl<T> TypeDef for &T
where
    T: TypeDef,
{
    type Deps = (T,);

    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: T::INFO.r#ref(),
    });
}

impl<T> TypeDef for &mut T
where
    T: TypeDef,
{
    type Deps = (T,);

    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: T::INFO.r#ref(),
    });
}

impl<T> TypeDef for Box<T>
where
    T: TypeDef,
{
    type Deps = (T,);

    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: T::INFO.r#ref(),
    });
}

impl<T> TypeDef for std::borrow::Cow<'_, T>
where
    T: Clone + TypeDef,
{
    type Deps = (T,);

    const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
        r#ref: T::INFO.r#ref(),
    });
}
