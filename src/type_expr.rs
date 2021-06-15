use std::fmt;

#[derive(Debug, Clone, Copy)]
pub enum TypeExpr {
    TypeName(TypeName),
    Tuple(Tuple),
    Array(Array),
    Union(Union),
}

#[derive(Debug, Clone, Copy)]
pub struct TypeName {
    pub name: &'static Ident,
    pub generics: &'static List<TypeExpr>,
}

#[derive(Debug, Clone, Copy)]
pub struct Tuple(pub &'static List<TypeExpr>);

#[derive(Debug, Clone, Copy)]
pub struct Array(pub &'static TypeExpr);

#[derive(Debug, Clone, Copy)]
pub struct Union(pub &'static List<TypeExpr>);

#[derive(Debug, Clone, Copy)]
pub struct Ident(pub &'static str);

pub type List<T> = [&'static T];

impl TypeExpr {
    pub const fn ident(ident: &'static Ident) -> Self {
        Self::TypeName(TypeName::ident(ident))
    }
}

impl TypeName {
    pub const fn ident(ident: &'static Ident) -> Self {
        Self {
            name: ident,
            generics: &[],
        }
    }
}

impl fmt::Display for TypeExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeExpr::TypeName(type_name) => write!(f, "{}", type_name),
            TypeExpr::Tuple(tuple) => write!(f, "{}", tuple),
            TypeExpr::Array(array) => write!(f, "{}", array),
            TypeExpr::Union(r#union) => write!(f, "{}", r#union),
        }
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

impl fmt::Display for Union {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        let mut first = true;
        for expr in self.0 {
            if !first {
                write!(f, "|")?;
            }
            write!(f, "{}", expr)?;
            first = false;
        }
        write!(f, ")")?;
        Ok(())
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
