use crate::type_expr::{
    DefinedTypeInfo, Ident, NativeTypeInfo, ObjectField, TypeArray,
    TypeDefinition, TypeExpr, TypeInfo, TypeIntersection, TypeName, TypeObject,
    TypeString, TypeTuple, TypeUnion,
};
use std::{
    collections::HashSet,
    hash::Hash,
    iter::{self, FusedIterator},
    slice,
};

/// An iterator which produces all type definitions that a type depends on.
///
/// Type definitions dependencies (including those of generic types) are
/// produced exactly once in post-order.
pub struct IterDefDeps {
    stack: Vec<TypeExpr>,
    visited: HashSet<u64>,
    emitted: HashSet<u64>,
}

impl IterDefDeps {
    /// Creates a new iterator of the dependencies of the given type info.
    pub fn new(root: &'static TypeInfo) -> Self {
        Self {
            stack: vec![TypeExpr::Ref(root)],
            visited: HashSet::new(),
            emitted: HashSet::new(),
        }
    }
}

impl Iterator for IterDefDeps {
    type Item = &'static TypeDefinition;

    fn next(&mut self) -> Option<Self::Item> {
        let Self {
            stack,
            visited,
            emitted,
        } = self;
        while let Some(expr) = stack.pop() {
            if TypeExprChildren::new(&expr).all(|child| {
                visited.contains(&hash_type_expr(child, HashKind::Visit))
            }) {
                let expr_visit_hash = hash_type_expr(&expr, HashKind::Visit);
                let expr_emit_hash = hash_type_expr(&expr, HashKind::Emit);
                if !emitted.contains(&expr_emit_hash) {
                    emitted.insert(expr_emit_hash);
                    if let TypeExpr::Ref(TypeInfo::Defined(DefinedTypeInfo {
                        def,
                        generic_args: _,
                    })) = expr
                    {
                        return Some(def);
                    }
                }
                visited.insert(expr_visit_hash);
            } else {
                stack.push(expr);
                stack.extend(
                    TypeExprChildren::new(&expr)
                        .filter(|expr| {
                            !visited.contains(&hash_type_expr(
                                expr,
                                HashKind::Visit,
                            ))
                        })
                        .rev(),
                );
            }
        }
        None
    }
}

impl FusedIterator for IterDefDeps {}

/// An iterator which produces all of the direct type expression children of a
/// type expression.
enum TypeExprChildren<'a> {
    None,
    One(iter::Once<&'a TypeExpr>),
    Slice(slice::Iter<'a, TypeExpr>),
    OneThenSlice(
        iter::Chain<iter::Once<&'a TypeExpr>, slice::Iter<'a, TypeExpr>>,
    ),
    Object(slice::Iter<'a, ObjectField>),
}

impl TypeExprChildren<'_> {
    fn new(expr: &TypeExpr) -> Self {
        match expr {
            TypeExpr::Ref(TypeInfo::Native(NativeTypeInfo { r#ref })) => {
                Self::One(iter::once(r#ref))
            }
            TypeExpr::Ref(TypeInfo::Defined(DefinedTypeInfo {
                def:
                    TypeDefinition {
                        docs: _,
                        path: _,
                        name: _,
                        generic_vars: _,
                        def,
                    },
                generic_args,
            })) => {
                Self::OneThenSlice(iter::once(def).chain(generic_args.iter()))
            }
            TypeExpr::Name(TypeName {
                path: _,
                name: _,
                generic_args,
            }) => Self::Slice(generic_args.iter()),
            TypeExpr::String(TypeString { docs: _, value: _ }) => Self::None,
            TypeExpr::Tuple(TypeTuple { docs: _, elements }) => {
                Self::Slice(elements.iter())
            }
            TypeExpr::Object(TypeObject { docs: _, fields }) => {
                Self::Object(fields.iter())
            }
            TypeExpr::Array(TypeArray { docs: _, item }) => {
                Self::One(iter::once(item))
            }
            TypeExpr::Union(TypeUnion { docs: _, members }) => {
                Self::Slice(members.iter())
            }
            TypeExpr::Intersection(TypeIntersection { docs: _, members }) => {
                Self::Slice(members.iter())
            }
        }
    }
}

impl<'a> Iterator for TypeExprChildren<'a> {
    type Item = &'a TypeExpr;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::None => None,
            Self::One(iter) => iter.next(),
            Self::Slice(iter) => iter.next(),
            Self::OneThenSlice(iter) => iter.next(),
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
            Self::OneThenSlice(iter) => iter.size_hint(),
            Self::Object(iter) => iter.size_hint(),
        }
    }
}

impl FusedIterator for TypeExprChildren<'_> {}

impl ExactSizeIterator for TypeExprChildren<'_> {}

impl DoubleEndedIterator for TypeExprChildren<'_> {
    fn next_back(&mut self) -> Option<<Self as Iterator>::Item> {
        match self {
            Self::None => None,
            Self::One(iter) => iter.next_back(),
            Self::Slice(iter) => iter.next_back(),
            Self::OneThenSlice(iter) => iter.next_back(),
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

#[derive(Clone, Copy)]
enum HashKind {
    Visit,
    Emit,
}

fn hash_type_expr(expr: &TypeExpr, hash_kind: HashKind) -> u64 {
    use std::{collections::hash_map::DefaultHasher, hash::Hasher};

    fn visit_expr(
        expr: &TypeExpr,
        hash_kind: HashKind,
        state: &mut DefaultHasher,
    ) {
        match expr {
            TypeExpr::Ref(TypeInfo::Native(NativeTypeInfo { r#ref })) => {
                visit_expr(r#ref, hash_kind, state);
            }
            TypeExpr::Ref(TypeInfo::Defined(DefinedTypeInfo {
                def:
                    TypeDefinition {
                        docs: _,
                        path,
                        name: Ident(name),
                        generic_vars,
                        def,
                    },
                generic_args,
            })) => {
                for Ident(path_part) in *path {
                    path_part.hash(state);
                }
                name.hash(state);
                for Ident(generic_var) in *generic_vars {
                    generic_var.hash(state);
                }
                visit_expr(def, hash_kind, state);
                match hash_kind {
                    HashKind::Visit => {
                        for generic_arg in *generic_args {
                            visit_expr(generic_arg, hash_kind, state);
                        }
                    }
                    HashKind::Emit => {}
                }
            }
            TypeExpr::Name(TypeName {
                path,
                name: Ident(name),
                generic_args,
            }) => {
                for Ident(path_part) in *path {
                    path_part.hash(state);
                }
                name.hash(state);
                for generic_arg in *generic_args {
                    visit_expr(generic_arg, hash_kind, state);
                }
            }
            TypeExpr::String(TypeString { docs: _, value }) => {
                value.hash(state);
            }
            TypeExpr::Tuple(TypeTuple { docs: _, elements }) => {
                for element in *elements {
                    visit_expr(element, hash_kind, state);
                }
            }
            TypeExpr::Object(TypeObject { docs: _, fields }) => {
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
                    visit_expr(r#type, hash_kind, state);
                }
            }
            TypeExpr::Array(TypeArray { docs: _, item }) => {
                visit_expr(item, hash_kind, state);
            }
            TypeExpr::Union(TypeUnion { docs: _, members }) => {
                for member in *members {
                    visit_expr(member, hash_kind, state);
                }
            }
            TypeExpr::Intersection(TypeIntersection { docs: _, members }) => {
                for member in *members {
                    visit_expr(member, hash_kind, state);
                }
            }
        }
    }

    let mut hasher = DefaultHasher::new();
    visit_expr(expr, hash_kind, &mut hasher);
    hasher.finish()
}
