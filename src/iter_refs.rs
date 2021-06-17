use crate::type_expr::{
    Array,
    DefinedTypeInfo,
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
use std::{collections::HashSet, hash::Hash, iter, slice};

struct IterRefs {
    stack: Vec<TypeExpr>,
    visited: HashSet<u64>,
}

enum TypeExprChildren<'a> {
    None,
    One(iter::Once<&'a TypeExpr>),
    Slice(slice::Iter<'a, TypeExpr>),
    Object(slice::Iter<'a, ObjectField>),
}

impl TypeInfo {
    pub(crate) fn iter_refs(
        &'static self,
    ) -> impl Iterator<Item = DefinedTypeInfo> {
        IterRefs::new(self)
    }
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
            if TypeExprChildren::new(&expr)
                .all(|child| visited.contains(&hash_type_expr(child)))
            {
                let expr_hash = hash_type_expr(&expr);
                if !visited.contains(&expr_hash) {
                    visited.insert(expr_hash);
                    if let TypeExpr::Ref(TypeInfo::Defined(type_info)) = expr {
                        return Some(*type_info);
                    }
                }
            } else {
                stack.push(expr);
                stack.extend(
                    TypeExprChildren::new(&expr)
                        .filter(|expr| {
                            !visited.contains(&hash_type_expr(&expr))
                        })
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
            Self::One(iter) => iter.next(),
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
            Self::One(iter) => iter.size_hint(),
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
            Self::One(iter) => iter.next_back(),
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
                    Self::One(iter::once(def))
                },
                TypeInfo::Defined(DefinedTypeInfo {
                    docs: _,
                    path: _,
                    name: _,
                    def,
                }) => Self::One(iter::once(def)),
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
            TypeExpr::Array(Array { docs: _, item }) => {
                Self::One(iter::once(item))
            },
            TypeExpr::Union(Union { docs: _, members }) => {
                Self::Slice(members.iter())
            },
            TypeExpr::Intersection(Intersection { docs: _, members }) => {
                Self::Slice(members.iter())
            },
        }
    }
}

fn hash_type_expr(expr: &TypeExpr) -> u64 {
    use std::{collections::hash_map::DefaultHasher, hash::Hasher};

    fn visit_expr(expr: &TypeExpr, state: &mut DefaultHasher) {
        match expr {
            TypeExpr::Ref(TypeInfo::Native(NativeTypeInfo { def })) => {
                visit_expr(def, state);
            },
            TypeExpr::Ref(TypeInfo::Defined(DefinedTypeInfo {
                docs: _,
                path,
                name: Ident(name),
                def,
            })) => {
                for Ident(path_part) in *path {
                    path_part.hash(state);
                }
                name.hash(state);
                visit_expr(def, state);
            },
            TypeExpr::Name(TypeName {
                path,
                name: Ident(name),
                generics,
            }) => {
                for Ident(path_part) in *path {
                    path_part.hash(state);
                }
                name.hash(state);
                for generic in *generics {
                    visit_expr(generic, state);
                }
            },
            TypeExpr::String(TypeString { docs: _, value }) => {
                value.hash(state);
            },
            TypeExpr::Tuple(Tuple { docs: _, elements }) => {
                for element in *elements {
                    visit_expr(element, state);
                }
            },
            TypeExpr::Object(Object { docs: _, fields }) => {
                for ObjectField {
                    docs: _,
                    name:
                        TypeString {
                            docs: _,
                            value: name,
                        },
                    optional,
                    r#type,
                } in *fields
                {
                    name.hash(state);
                    optional.hash(state);
                    visit_expr(r#type, state);
                }
            },
            TypeExpr::Array(Array { docs: _, item }) => {
                visit_expr(item, state);
            },
            TypeExpr::Union(Union { docs: _, members }) => {
                for member in *members {
                    visit_expr(member, state);
                }
            },
            TypeExpr::Intersection(Intersection { docs: _, members }) => {
                for member in *members {
                    visit_expr(member, state);
                }
            },
        }
    }

    let mut hasher = DefaultHasher::new();
    visit_expr(expr, &mut hasher);
    hasher.finish()
}
