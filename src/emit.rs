use crate::type_info::{CustomTypeInfo, NativeTypeInfo, TypeInfo};
use std::{any::TypeId, collections::HashSet, io, io::Write};

pub trait TypeDef: 'static {
    type Deps: Deps;

    const INFO: TypeInfo;
}

pub struct EmitCtx<'ctx> {
    w: &'ctx mut dyn io::Write,
    visited: HashSet<TypeId>,
}

impl io::Write for EmitCtx<'_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.w.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.w.flush()
    }
}

pub trait Deps: private::Sealed {
    fn emit_each(ctx: &mut EmitCtx<'_>) -> io::Result<()>;
}

impl<'ctx> EmitCtx<'ctx> {
    pub(crate) fn emit_type<T>(&mut self) -> io::Result<()>
    where
        T: TypeDef,
    {
        // TODO: can remove 'static requirement by using std::any::type_name?
        // it might not be unique though
        let type_id = TypeId::of::<T>();
        if !self.visited.contains(&type_id) {
            self.visited.insert(type_id);
            self.emit_def::<T>()?;
            <T::Deps as Deps>::emit_each(self)?;
        }
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

pub(crate) mod private {
    pub trait Sealed {}
}
