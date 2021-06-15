use crate::type_expr::{CustomTypeInfo, NativeTypeInfo, TypeInfo, TypeName};
use std::{any::TypeId, collections::HashSet, io, io::Write};

pub trait TypeDef: 'static {
    type Deps: Deps;

    const INFO: TypeInfo;
}

pub struct EmitCtx<'ctx> {
    w: &'ctx mut dyn io::Write,
    visited: HashSet<TypeId>,
}

pub trait Deps: private::Sealed {
    fn emit_each(ctx: &mut EmitCtx<'_>) -> io::Result<()>;
}

pub(crate) trait Emit {
    fn emit(&self, ctx: &mut EmitCtx<'_>) -> io::Result<()>;
}

impl<'ctx> EmitCtx<'ctx> {
    fn new(w: &'ctx mut dyn io::Write) -> Self {
        Self {
            w,
            visited: HashSet::new(),
        }
    }

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
            TypeInfo::Custom(CustomTypeInfo {
                name:
                    TypeName {
                        path,
                        name,
                        generics,
                    },
                def,
            }) => {
                if !path.is_empty() {
                    write!(self, "export namespace ")?;
                    let mut first = true;
                    for path_part in *path {
                        if !first {
                            write!(self, ".")?;
                        }
                        path_part.emit(self)?;
                        first = false;
                    }
                    writeln!(self, "{{")?;
                }
                write!(self, "export type ")?;
                TypeName {
                    path: &[],
                    name,
                    generics,
                }
                .emit(self)?;
                write!(self, "=")?;
                def.emit(self)?;
                write!(self, ";")?;
                if !path.is_empty() {
                    write!(self, "}}")?;
                }
                writeln!(self)?;
                Ok(())
            },
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

pub fn write_definition_file<W, T>(mut writer: W) -> io::Result<()>
where
    W: io::Write,
    T: TypeDef,
{
    let w = &mut writer;
    writeln!(w, "// AUTO-GENERATED by typescript-type-def")?;
    writeln!(w)?;
    writeln!(w, "export default types;")?;
    writeln!(w, "export namespace types{{")?;
    EmitCtx::new(w).emit_type::<T>()?;
    writeln!(w, "}}")?;
    Ok(())
}

pub(crate) mod private {
    pub trait Sealed {}
}