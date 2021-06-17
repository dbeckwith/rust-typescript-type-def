use crate::type_expr::{
    Array,
    DefinedTypeInfo,
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
use std::collections::HashSet;

impl TypeInfo {
    pub(crate) fn iter_refs(
        &'static self,
    ) -> impl Iterator<Item = DefinedTypeInfo> {
        IterRefs::new(self)
    }
}

struct IterRefs {
    stack: Vec<TypeExpr>,
    visited: HashSet<TypeExpr>,
}

enum TypeExprChildren<'a> {
    None,
    One(Option<&'a TypeExpr>),
    Slice(std::slice::Iter<'a, TypeExpr>),
    Object(std::slice::Iter<'a, ObjectField>),
}

impl IterRefs {
    fn new(root: &'static TypeInfo) -> Self {
        Self {
            stack: vec![TypeExpr::Ref(root)],
            visited: HashSet::new(),
        }
    }
}

impl Iterator for IterRefs {
    type Item = DefinedTypeInfo;

    fn next(&mut self) -> Option<Self::Item> {
        let Self { stack, visited } = self;
        while let Some(expr) = stack.pop() {
            if TypeExprChildren::new(&expr).all(|child| visited.contains(child))
            {
                if !visited.contains(&expr) {
                    visited.insert(expr);
                    if let TypeExpr::Ref(TypeInfo::Defined(type_info)) = expr {
                        return Some(*type_info);
                    }
                }
            } else {
                stack.push(expr);
                stack.extend(
                    TypeExprChildren::new(&expr)
                        .filter(|expr| !visited.contains(expr))
                        .rev(),
                );
            }
        }
        None
    }
}

impl<'a> Iterator for TypeExprChildren<'a> {
    type Item = &'a TypeExpr;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::None => None,
            Self::One(item) => item.take(),
            Self::Slice(iter) => iter.next(),
            Self::Object(iter) => iter.next().map(
                |ObjectField {
                     docs: _,
                     name: _,
                     optional: _,
                     r#type,
                 }| { r#type },
            ),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::None => (0, Some(0)),
            Self::One(item) => {
                if item.is_some() {
                    (1, Some(1))
                } else {
                    (0, Some(0))
                }
            },
            Self::Slice(iter) => iter.size_hint(),
            Self::Object(iter) => iter.size_hint(),
        }
    }
}

impl ExactSizeIterator for TypeExprChildren<'_> {}

impl DoubleEndedIterator for TypeExprChildren<'_> {
    fn next_back(&mut self) -> Option<<Self as Iterator>::Item> {
        match self {
            Self::None => None,
            Self::One(item) => item.take(),
            Self::Slice(iter) => iter.next_back(),
            Self::Object(iter) => iter.next_back().map(
                |ObjectField {
                     docs: _,
                     name: _,
                     optional: _,
                     r#type,
                 }| { r#type },
            ),
        }
    }
}

impl TypeExprChildren<'_> {
    fn new(expr: &TypeExpr) -> Self {
        match expr {
            TypeExpr::Ref(type_info) => match type_info {
                TypeInfo::Native(NativeTypeInfo { def }) => {
                    Self::One(Some(def))
                },
                TypeInfo::Defined(DefinedTypeInfo {
                    docs: _,
                    path: _,
                    name: _,
                    def,
                }) => Self::One(Some(def)),
            },
            TypeExpr::Name(TypeName {
                path: _,
                name: _,
                generics,
            }) => Self::Slice(generics.iter()),
            TypeExpr::String(TypeString { docs: _, value: _ }) => Self::None,
            TypeExpr::Tuple(Tuple { docs: _, elements }) => {
                Self::Slice(elements.iter())
            },
            TypeExpr::Object(Object { docs: _, fields }) => {
                Self::Object(fields.iter())
            },
            TypeExpr::Array(Array { docs: _, item }) => Self::One(Some(item)),
            TypeExpr::Union(Union { docs: _, members }) => {
                Self::Slice(members.iter())
            },
            TypeExpr::Intersection(Intersection { docs: _, members }) => {
                Self::Slice(members.iter())
            },
        }
    }
}
