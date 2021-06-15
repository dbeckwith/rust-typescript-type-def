#![warn(rust_2018_idioms, clippy::all)]
#![deny(clippy::correctness)]

pub mod lib2;

use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    hash::Hash,
    io,
};

/// Derive macro for [`trait@TypescriptTypeDef`].
// TODO: document attributes and usage
pub use typescript_type_def_derive::TypescriptTypeDef;

/// A trait for types which can emit a Typescript type definition compatible
/// with their JSON serialization.
pub trait TypescriptTypeDef: 'static {
    /// Whether the top of this type is a native Typescript type or not.
    ///
    /// Native Typescript types are types that do not require a definition. In
    /// other words, [`emit_name`](TypescriptTypeDef::emit_name) emits a full
    /// description of the type instead of a type alias.
    ///
    /// If this constant is `true`, [`emit_name`](TypescriptTypeDef::emit_name)
    /// should be a no-op.
    const NATIVE: bool = false;

    /// Emit the name of this type.
    ///
    /// This method should emit the fully-qualified name of this type in the
    /// context of the definitions file. If this is a non-native type, the name
    /// should be a namespaced identifier. If this is a native type, it may be a
    /// full Typescript type expression.
    fn emit_name(ctx: &mut Context<'_>) -> io::Result<()>;

    /// Emit the dependencies of this type.
    ///
    /// This method should call [`EmitDepsContext::emit_dep`] for all types
    /// referenced in the definition or name of this type to ensure their
    /// definitions are emitted as well.
    fn emit_deps(ctx: &mut EmitDepsContext<'_>) -> io::Result<()>;

    /// Emit the definition of this type.
    ///
    /// This method should emit a type alias statement declaring the definition
    /// of this type. The statement should be of the form `export type {NAME} =
    /// {TYPE_EXPRESSION};` where `{NAME}` is the type's name (you may use
    /// [`emit_name`](TypescriptTypeDef::emit_name) for this) and
    /// `{TYPE_EXPRESSION}` is the definition.
    ///
    /// This method should be a no-op for native types.
    fn emit_def(ctx: &mut Context<'_>) -> io::Result<()>;
}

pub struct Context<'a> {
    emitted_types: HashSet<String>,
    dep_stack: HashSet<String>,
    pub out: &'a mut dyn io::Write,
}

impl Context<'_> {
    pub fn emit_name<T>(&mut self) -> io::Result<()>
    where
        T: TypescriptTypeDef,
    {
        if !T::NATIVE {
            write!(self.out, "types.")?;
        }
        T::emit_name(self)?;
        Ok(())
    }
}

pub struct EmitDepsContext<'a> {
    ctx: Context<'a>,
}

impl<'a> EmitDepsContext<'a> {
    pub fn new(out: &'a mut dyn io::Write) -> Self {
        Self {
            ctx: Context {
                emitted_types: Default::default(),
                dep_stack: Default::default(),
                out,
            },
        }
    }
}

impl EmitDepsContext<'_> {
    pub fn emit_dep<T>(&mut self) -> io::Result<()>
    where
        T: TypescriptTypeDef,
    {
        // TODO: detect name collisions from different types

        let mut type_id = std::any::type_name::<T>().to_owned();

        // prevent cycles by seeing if this type has already been visited in the
        // current stack
        if self.ctx.dep_stack.contains(&type_id) {
            return Ok(());
        }
        self.ctx.dep_stack.insert(type_id.clone());

        // remove generic params from type id
        if let Some(idx) = type_id.find('<') {
            type_id.truncate(idx);
        }

        // emit deps for every visit of the type, since the deps may depend on
        // generic params
        T::emit_deps(self)?;

        // only emit the definition of each type id exactly once
        if !self.ctx.emitted_types.contains(&type_id) {
            self.ctx.emitted_types.insert(type_id.clone());
            if !T::NATIVE {
                write!(self.ctx.out, "export namespace types{{")?;
                T::emit_def(&mut self.ctx)?;
                write!(self.ctx.out, "}}")?;
            }
        }

        // pop the stack
        self.ctx.dep_stack.remove(&type_id);

        Ok(())
    }
}

pub struct Blob(pub Vec<u8>);

macro_rules! impl_native {
    ($ty:ty, $ts_ty:literal) => {
        impl TypescriptTypeDef for $ty {
            const NATIVE: bool = true;

            fn emit_name(ctx: &mut Context<'_>) -> io::Result<()> {
                write!(ctx.out, "{}", $ts_ty)
            }

            fn emit_deps(_ctx: &mut EmitDepsContext<'_>) -> io::Result<()> {
                Ok(())
            }

            fn emit_def(_ctx: &mut Context<'_>) -> io::Result<()> {
                Ok(())
            }
        }
    };
}

impl_native!(Blob, "Blob");
impl_native!(bool, "boolean");
impl_native!(String, "string");
impl_native!(&'static str, "string");

macro_rules! impl_alias {
    ($ty:ty, $name:ident, $ts_ty:literal) => {
        impl TypescriptTypeDef for $ty {
            fn emit_name(ctx: &mut Context<'_>) -> io::Result<()> {
                write!(ctx.out, "{}", stringify!($name))
            }

            fn emit_deps(_ctx: &mut EmitDepsContext<'_>) -> io::Result<()> {
                Ok(())
            }

            fn emit_def(ctx: &mut Context<'_>) -> io::Result<()> {
                write!(ctx.out, "export type {}={};", stringify!($name), $ts_ty)
            }
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

macro_rules! impl_tuple {
    ($($var:ident),+) => {
        impl<$($var),+> TypescriptTypeDef for ($($var,)+)
        where
            $($var: TypescriptTypeDef,)+
        {
            const NATIVE: bool = true;

            fn emit_name(ctx: &mut Context<'_>) -> io::Result<()> {
                write!(ctx.out, "[")?;
                impl_tuple!(@emit_name ctx; $($var)+);
                write!(ctx.out, "]")?;
                Ok(())
            }

            fn emit_deps(ctx: &mut EmitDepsContext<'_>) -> io::Result<()> {
                $(ctx.emit_dep::<$var>()?;)+
                Ok(())
            }

            fn emit_def(_ctx: &mut Context<'_>) -> io::Result<()> {
                Ok(())
            }
        }
    };
    (@emit_name $ctx:expr; $var:ident) => {
        $ctx.emit_name::<$var>()?;
    };
    (@emit_name $ctx:expr; $var:ident $($tail:ident)+) => {
        $ctx.emit_name::<$var>()?;
        write!($ctx.out, ",")?;
        impl_tuple!(@emit_name $ctx; $($tail)+);
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

impl<'a, B: ?Sized> TypescriptTypeDef for Cow<'a, B>
where
    &'a B: TypescriptTypeDef,
    B: ToOwned,
{
    const NATIVE: bool = <&'a B>::NATIVE;

    fn emit_name(ctx: &mut Context<'_>) -> io::Result<()> {
        ctx.emit_name::<&'a B>()?;
        Ok(())
    }

    fn emit_deps(ctx: &mut EmitDepsContext<'_>) -> io::Result<()> {
        ctx.emit_dep::<&'a B>()?;
        Ok(())
    }

    fn emit_def(_ctx: &mut Context<'_>) -> io::Result<()> {
        Ok(())
    }
}

impl<T> TypescriptTypeDef for Box<T>
where
    T: TypescriptTypeDef,
{
    const NATIVE: bool = true;

    fn emit_name(ctx: &mut Context<'_>) -> io::Result<()> {
        ctx.emit_name::<T>()?;
        Ok(())
    }

    fn emit_deps(ctx: &mut EmitDepsContext<'_>) -> io::Result<()> {
        ctx.emit_dep::<T>()?;
        Ok(())
    }

    fn emit_def(_ctx: &mut Context<'_>) -> io::Result<()> {
        Ok(())
    }
}

impl<T> TypescriptTypeDef for Option<T>
where
    T: TypescriptTypeDef,
{
    const NATIVE: bool = true;

    fn emit_name(ctx: &mut Context<'_>) -> io::Result<()> {
        write!(ctx.out, "(")?;
        ctx.emit_name::<T>()?;
        write!(ctx.out, "|null)")?;
        Ok(())
    }

    fn emit_deps(ctx: &mut EmitDepsContext<'_>) -> io::Result<()> {
        ctx.emit_dep::<T>()?;
        Ok(())
    }

    fn emit_def(_ctx: &mut Context<'_>) -> io::Result<()> {
        Ok(())
    }
}

impl<T> TypescriptTypeDef for Vec<T>
where
    T: TypescriptTypeDef,
{
    const NATIVE: bool = true;

    fn emit_name(ctx: &mut Context<'_>) -> io::Result<()> {
        write!(ctx.out, "(")?;
        ctx.emit_name::<T>()?;
        write!(ctx.out, "[])")?;
        Ok(())
    }

    fn emit_deps(ctx: &mut EmitDepsContext<'_>) -> io::Result<()> {
        ctx.emit_dep::<T>()?;
        Ok(())
    }

    fn emit_def(_ctx: &mut Context<'_>) -> io::Result<()> {
        Ok(())
    }
}

impl<T> TypescriptTypeDef for HashSet<T>
where
    T: TypescriptTypeDef + Eq + Hash,
{
    const NATIVE: bool = true;

    fn emit_name(ctx: &mut Context<'_>) -> io::Result<()> {
        write!(ctx.out, "Set<")?;
        ctx.emit_name::<T>()?;
        write!(ctx.out, ">")?;
        Ok(())
    }

    fn emit_deps(ctx: &mut EmitDepsContext<'_>) -> io::Result<()> {
        ctx.emit_dep::<T>()?;
        Ok(())
    }

    fn emit_def(_ctx: &mut Context<'_>) -> io::Result<()> {
        Ok(())
    }
}

impl<K, V> TypescriptTypeDef for HashMap<K, V>
where
    K: TypescriptTypeDef + Eq + Hash,
    V: TypescriptTypeDef,
{
    const NATIVE: bool = true;

    fn emit_name(ctx: &mut Context<'_>) -> io::Result<()> {
        write!(ctx.out, "Record<")?;
        ctx.emit_name::<K>()?;
        write!(ctx.out, ",")?;
        ctx.emit_name::<V>()?;
        write!(ctx.out, ">")?;
        Ok(())
    }

    fn emit_deps(ctx: &mut EmitDepsContext<'_>) -> io::Result<()> {
        ctx.emit_dep::<K>()?;
        ctx.emit_dep::<V>()?;
        Ok(())
    }

    fn emit_def(_ctx: &mut Context<'_>) -> io::Result<()> {
        Ok(())
    }
}
