use crate::type_expr::{
    Array,
    DefinedTypeInfo,
    Docs,
    Ident,
    Intersection,
    NativeTypeInfo,
    Object,
    ObjectField,
    Tuple,
    TypeExpr,
    TypeInfo,
    TypeName,
    TypeString,
    Union,
};
use std::{any::TypeId, collections::HashSet, io, io::Write};

/// A Rust type that has a corresponding TypeScript type definition.
///
/// For a Rust type `T`, the `TypeDef` trait defines a TypeScript type which
/// describes JavaScript value that are equivalents of Rust values of type `T`
/// as encoded to JSON using [`serde_json`](https://docs.rs/serde_json/). The
/// types are one-to-one, so decoding from TypeScript to JSON to Rust also
/// works.
///
/// You should use [`#[derive(TypeDef)]`](macro@crate::TypeDef) macro to
/// implement this trait on your own types.
pub trait TypeDef: 'static {
    // TODO: automatically infer deps using all TypeExpr::Refs in INFO
    type Deps: Deps;

    /// A constant value describing the structure of this type.
    ///
    /// This type information is used to emit a TypeScript type definition.
    const INFO: TypeInfo;
}

// TODO: try to make this private
pub struct EmitCtx<'ctx> {
    w: &'ctx mut dyn io::Write,
    visited: HashSet<TypeId>,
    stats: Stats,
}

pub trait Deps: private::Sealed {
    fn emit_each(ctx: &mut EmitCtx<'_>) -> io::Result<()>;
}

pub(crate) trait Emit {
    fn emit(&self, ctx: &mut EmitCtx<'_>) -> io::Result<()>;
}

#[derive(Debug, Clone, Default)]
pub struct Stats {
    pub type_definitions: usize,
}

impl<'ctx> EmitCtx<'ctx> {
    fn new(w: &'ctx mut dyn io::Write) -> Self {
        Self {
            w,
            visited: Default::default(),
            stats: Default::default(),
        }
    }
}

impl io::Write for EmitCtx<'_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.w.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.w.flush()
    }
}

impl Emit for TypeExpr {
    fn emit(&self, ctx: &mut EmitCtx<'_>) -> io::Result<()> {
        match self {
            TypeExpr::Ref(type_info) => match type_info {
                TypeInfo::Native(NativeTypeInfo { r#ref }) => r#ref.emit(ctx),
                TypeInfo::Defined(DefinedTypeInfo {
                    docs: _,
                    name,
                    def: _,
                }) => {
                    write!(ctx, "types.")?;
                    name.emit(ctx)
                },
            },
            TypeExpr::Name(type_name) => type_name.emit(ctx),
            TypeExpr::String(type_string) => type_string.emit(ctx),
            TypeExpr::Tuple(tuple) => tuple.emit(ctx),
            TypeExpr::Object(object) => object.emit(ctx),
            TypeExpr::Array(array) => array.emit(ctx),
            TypeExpr::Union(r#union) => r#union.emit(ctx),
            TypeExpr::Intersection(intersection) => intersection.emit(ctx),
        }
    }
}

impl Emit for TypeName {
    fn emit(&self, ctx: &mut EmitCtx<'_>) -> io::Result<()> {
        let Self {
            docs,
            path,
            name,
            generics,
        } = self;
        docs.emit(ctx)?;
        for path_part in *path {
            path_part.emit(ctx)?;
            write!(ctx, ".")?;
        }
        name.emit(ctx)?;
        if !generics.is_empty() {
            write!(ctx, "<")?;
            let mut first = true;
            for generic in *generics {
                if !first {
                    write!(ctx, ",")?;
                }
                generic.emit(ctx)?;
                first = false;
            }
            write!(ctx, ">")?;
        }
        Ok(())
    }
}

impl Emit for TypeString {
    fn emit(&self, ctx: &mut EmitCtx<'_>) -> io::Result<()> {
        let Self { docs, value } = self;
        docs.emit(ctx)?;
        write!(ctx, "{:?}", value)?;
        Ok(())
    }
}

impl Emit for Tuple {
    fn emit(&self, ctx: &mut EmitCtx<'_>) -> io::Result<()> {
        let Self { docs, elements } = self;
        docs.emit(ctx)?;
        write!(ctx, "[")?;
        let mut first = true;
        for element in *elements {
            if !first {
                write!(ctx, ",")?;
            }
            element.emit(ctx)?;
            first = false;
        }
        write!(ctx, "]")?;
        Ok(())
    }
}

impl Emit for Object {
    fn emit(&self, ctx: &mut EmitCtx<'_>) -> io::Result<()> {
        let Self { docs, fields } = self;
        docs.emit(ctx)?;
        write!(ctx, "{{")?;
        for ObjectField {
            docs,
            name,
            optional,
            r#type,
        } in *fields
        {
            docs.emit(ctx)?;
            name.emit(ctx)?;
            if *optional {
                write!(ctx, "?")?;
            }
            write!(ctx, ":")?;
            r#type.emit(ctx)?;
            write!(ctx, ";")?;
        }
        write!(ctx, "}}")?;
        Ok(())
    }
}

impl Emit for Array {
    fn emit(&self, ctx: &mut EmitCtx<'_>) -> io::Result<()> {
        let Self { docs, item } = self;
        docs.emit(ctx)?;
        write!(ctx, "(")?;
        item.emit(ctx)?;
        write!(ctx, ")[]")?;
        Ok(())
    }
}

impl Emit for Union {
    fn emit(&self, ctx: &mut EmitCtx<'_>) -> io::Result<()> {
        let Self { docs, parts } = self;
        docs.emit(ctx)?;
        if parts.is_empty() {
            write!(ctx, "never")?;
        } else {
            write!(ctx, "(")?;
            let mut first = true;
            for part in *parts {
                if !first {
                    write!(ctx, "|")?;
                }
                part.emit(ctx)?;
                first = false;
            }
            write!(ctx, ")")?;
        }
        Ok(())
    }
}

impl Emit for Intersection {
    fn emit(&self, ctx: &mut EmitCtx<'_>) -> io::Result<()> {
        let Self { docs, parts } = self;
        docs.emit(ctx)?;
        if parts.is_empty() {
            write!(ctx, "any")?;
        } else {
            write!(ctx, "(")?;
            let mut first = true;
            for part in *parts {
                if !first {
                    write!(ctx, "&")?;
                }
                part.emit(ctx)?;
                first = false;
            }
            write!(ctx, ")")?;
        }
        Ok(())
    }
}

impl Emit for Ident {
    fn emit(&self, ctx: &mut EmitCtx<'_>) -> io::Result<()> {
        let Self(name) = self;
        write!(ctx, "{}", name)?;
        Ok(())
    }
}

impl Emit for Docs {
    fn emit(&self, ctx: &mut EmitCtx<'_>) -> io::Result<()> {
        let Self(docs) = self;
        writeln!(ctx)?;
        writeln!(ctx, "/**")?;
        for line in docs.split('\n') {
            writeln!(ctx, " * {}", line)?;
        }
        writeln!(ctx, " */")?;
        Ok(())
    }
}

impl<T> Emit for &T
where
    T: Emit,
{
    fn emit(&self, ctx: &mut EmitCtx<'_>) -> io::Result<()> {
        T::emit(self, ctx)
    }
}

impl<T> Emit for Option<T>
where
    T: Emit,
{
    fn emit(&self, ctx: &mut EmitCtx<'_>) -> io::Result<()> {
        if let Some(inner) = self {
            inner.emit(ctx)
        } else {
            Ok(())
        }
    }
}

impl EmitCtx<'_> {
    pub(crate) fn emit_type<T>(&mut self) -> io::Result<()>
    where
        T: TypeDef,
    {
        // TODO: can remove 'static requirement by using std::any::type_name?
        // it might not be unique though
        let type_id = TypeId::of::<T>();
        if !self.visited.contains(&type_id) {
            self.visited.insert(type_id);
            <T::Deps as Deps>::emit_each(self)?;
            self.emit_def::<T>()?;
        }
        Ok(())
    }

    fn emit_def<T>(&mut self) -> io::Result<()>
    where
        T: TypeDef,
    {
        match T::INFO {
            TypeInfo::Native(NativeTypeInfo { r#ref: _ }) => Ok(()),
            TypeInfo::Defined(DefinedTypeInfo { docs, name, def }) => {
                self.stats.type_definitions += 1;
                docs.emit(self)?;
                if !name.path.is_empty() {
                    write!(self, "export namespace ")?;
                    let mut first = true;
                    for path_part in name.path {
                        if !first {
                            write!(self, ".")?;
                        }
                        path_part.emit(self)?;
                        first = false;
                    }
                    writeln!(self, "{{")?;
                }
                write!(self, "export type ")?;
                TypeName { path: &[], ..*name }.emit(self)?;
                write!(self, "=")?;
                def.emit(self)?;
                write!(self, ";")?;
                if !name.path.is_empty() {
                    write!(self, "}}")?;
                }
                writeln!(self)?;
                Ok(())
            },
        }
    }
}

pub fn write_definition_file<W, T>(mut writer: W) -> io::Result<Stats>
where
    W: io::Write,
    T: TypeDef,
{
    let w = &mut writer;
    writeln!(w, "// AUTO-GENERATED by typescript-type-def")?;
    writeln!(w)?;
    writeln!(w, "export default types;")?;
    writeln!(w, "export namespace types{{")?;
    let mut ctx = EmitCtx::new(w);
    ctx.emit_type::<T>()?;
    let stats = ctx.stats;
    writeln!(w, "}}")?;
    Ok(stats)
}

pub(crate) mod private {
    pub trait Sealed {}
}
