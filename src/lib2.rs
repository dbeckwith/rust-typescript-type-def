use std::{fmt, io, io::Write};

pub type List<T> = [&'static T];
#[derive(Debug, Clone, Copy)]
pub struct Ident(pub &'static str);
#[derive(Debug, Clone, Copy)]
pub struct TypeName {
    pub name: &'static Ident,
    pub generics: &'static List<TypeExpr>,
}
#[derive(Debug, Clone, Copy)]
pub struct Tuple(pub &'static List<TypeExpr>);
#[derive(Debug, Clone, Copy)]
pub struct Array(pub &'static &'static TypeExpr);
#[derive(Debug, Clone, Copy)]
pub enum TypeExpr {
    TypeName(TypeName),
    Tuple(Tuple),
    Array(Array),
}
#[derive(Debug, Clone, Copy)]
pub enum TypeInfo {
    Native(NativeTypeInfo),
    Custom(CustomTypeInfo),
}
#[derive(Debug, Clone, Copy)]
pub struct NativeTypeInfo {
    pub r#ref: &'static TypeExpr,
}
#[derive(Debug, Clone, Copy)]
pub struct CustomTypeInfo {
    pub path: &'static List<Ident>,
    pub name: &'static TypeName,
    pub def: &'static TypeExpr,
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.generics.is_empty() {
            write!(f, "<")?;
            let mut first = true;
            for expr in self.generics {
                if !first {
                    write!(f, ",")?;
                }
                write!(f, "{}", expr)?;
                first = false;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl fmt::Display for Tuple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        let mut first = true;
        for expr in self.0 {
            if !first {
                write!(f, ",")?;
            }
            write!(f, "{}", expr)?;
            first = false;
        }
        write!(f, "]")?;
        Ok(())
    }
}

impl fmt::Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({})[]", self.0)
    }
}

impl fmt::Display for TypeExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeExpr::TypeName(type_name) => write!(f, "{}", type_name),
            TypeExpr::Tuple(tuple) => write!(f, "{}", tuple),
            TypeExpr::Array(array) => write!(f, "{}", array),
        }
    }
}

impl TypeInfo {
    pub const fn r#ref(&'static self) -> &'static TypeExpr {
        match self {
            TypeInfo::Native(NativeTypeInfo { r#ref })
            | TypeInfo::Custom(CustomTypeInfo {
                path: _,
                name: _,
                def: r#ref,
            }) => r#ref,
        }
    }
}

pub trait TypeDef {
    type Deps: Deps;

    const INFO: TypeInfo;
}

pub struct EmitCtx<'ctx> {
    w: &'ctx mut dyn io::Write,
}

impl io::Write for EmitCtx<'_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.w.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.w.flush()
    }
}

pub trait Deps {
    fn emit_each(ctx: &mut EmitCtx<'_>) -> io::Result<()>;
}

impl<'ctx> EmitCtx<'ctx> {
    fn emit_type<T>(&mut self) -> io::Result<()>
    where
        T: TypeDef,
    {
        // TODO: de-dupe
        self.emit_def::<T>()?;
        <T::Deps as Deps>::emit_each(self)?;
        Ok(())
    }

    fn emit_def<T>(&mut self) -> io::Result<()>
    where
        T: TypeDef,
    {
        match T::INFO {
            TypeInfo::Native(NativeTypeInfo { r#ref: _ }) => Ok(()),
            TypeInfo::Custom(CustomTypeInfo { path, name, def }) => {
                write!(self, "type ")?;
                for path_part in path {
                    write!(self, "{}.", path_part)?;
                }
                writeln!(self, "{}={};", name, def)?;
                Ok(())
            },
        }
    }
}

macro_rules! impl_native {
    ($ty:ty, $ts_ty:literal) => {
        impl TypeDef for $ty {
            type Deps = ();

            const INFO: TypeInfo = TypeInfo::Native(NativeTypeInfo {
                r#ref: &TypeExpr::TypeName(TypeName {
                    name: &Ident($ts_ty),
                    generics: &[],
                }),
            });
        }
    };
}

pub struct Blob(pub Vec<u8>);
impl_native!(Blob, "Blob");
impl_native!(bool, "boolean");
impl_native!(String, "string");
impl_native!(&'static str, "string");

macro_rules! impl_alias {
    ($ty:ty, $name:ident, $ts_ty:literal) => {
        impl TypeDef for $ty {
            type Deps = ();

            const INFO: TypeInfo = TypeInfo::Custom(CustomTypeInfo {
                path: &[],
                name: &TypeName {
                    name: &Ident(stringify!($name)),
                    generics: &[],
                },
                def: &TypeExpr::TypeName(TypeName {
                    name: &Ident($ts_ty),
                    generics: &[],
                }),
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