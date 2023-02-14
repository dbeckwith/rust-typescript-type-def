//! This crate defines a procedural derive macro for implementing the `TypeDef`
//! trait from the `typescript_type_def` crate.
//!
//! See the documentation of that crate for more information.
#![warn(rust_2018_idioms, clippy::all)]
#![deny(clippy::correctness)]
#![allow(clippy::match_like_matches_macro)]

use darling::{
    ast,
    util::{Ignored, SpannedValue},
    FromDeriveInput, FromField, FromMeta, FromVariant,
};
use proc_macro2::Span;
use proc_macro_error::{abort, proc_macro_error};
use quote::{format_ident, quote};
use std::{ops::Deref, str::FromStr};
use syn::{
    ext::IdentExt,
    parse::Parser,
    parse_quote, parse_str,
    punctuated::Punctuated,
    visit_mut::{self, VisitMut},
    AngleBracketedGenericArguments, Attribute, DeriveInput, Expr,
    GenericArgument, GenericParam, Generics, Ident, Item, ItemImpl, ItemStruct,
    Lifetime, Lit, LitStr, Meta, MetaNameValue, Path, PathArguments,
    PathSegment, PredicateLifetime, PredicateType, Token, TraitBound,
    TraitBoundModifier, Type, TypeParam, TypeParamBound, TypePath, WhereClause,
    WherePredicate,
};

#[proc_macro_error]
#[proc_macro_derive(TypeDef, attributes(type_def, serde))]
pub fn derive_type_def(
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = match syn::parse2::<DeriveInput>(input.into()) {
        Ok(data) => data,
        Err(err) => return err.to_compile_error().into(),
    };
    let mut input = match TypeDefInput::from_derive_input(&input) {
        Ok(input) => input,
        Err(error) => return error.write_errors().into(),
    };

    remove_skipped(&mut input.data);

    let generics: &mut Generics = &mut input.generics;
    if generics.params.iter().any(|param| match param {
        GenericParam::Type(_) | GenericParam::Lifetime(_) => true,
        _ => false,
    }) {
        // add generic bounds
        generics
            .where_clause
            .get_or_insert_with(|| WhereClause {
                where_token: <Token![where]>::default(),
                predicates: Punctuated::new(),
            })
            .predicates
            .extend(generics.params.iter().filter_map(|param| {
                match param {
                    GenericParam::Type(param) => {
                        // all type params should impl TypeDef
                        Some(WherePredicate::Type(PredicateType {
                            lifetimes: None,
                            bounded_ty: Type::Path(TypePath {
                                qself: None,
                                path: ident_path(param.ident.clone()),
                            }),
                            colon_token: <Token![:]>::default(),
                            bounds: IntoIterator::into_iter([
                                TypeParamBound::Trait(TraitBound {
                                    paren_token: None,
                                    modifier: TraitBoundModifier::None,
                                    lifetimes: None,
                                    path: Path::parse_mod_style
                                        .parse_str(
                                            "::typescript_type_def::TypeDef",
                                        )
                                        .unwrap(),
                                }),
                            ])
                            .collect(),
                        }))
                    }
                    GenericParam::Lifetime(param) => {
                        // all lifetime params should be static
                        Some(WherePredicate::Lifetime(PredicateLifetime {
                            lifetime: param.lifetime.clone(),
                            colon_token: <Token![:]>::default(),
                            bounds: std::iter::once(Lifetime::new(
                                "'static",
                                Span::call_site(),
                            ))
                            .collect(),
                        }))
                    }
                    GenericParam::Const(_) => None,
                }
            }));
    }

    let ty_name = &input.ident;

    let (impl_generics, ty_generics, where_clause) =
        input.generics.split_for_impl();

    let info_def = make_info_def(&input);

    (quote! {
        impl #impl_generics ::typescript_type_def::TypeDef for
            #ty_name #ty_generics
        #where_clause
        {
            const INFO: ::typescript_type_def::type_expr::TypeInfo = #info_def;
        }
    })
    .into()
}

#[derive(FromDeriveInput)]
#[darling(attributes(type_def, serde), forward_attrs)]
struct TypeDefInput {
    attrs: Vec<Attribute>,
    ident: Ident,
    generics: Generics,
    data: ast::Data<TypeDefVariant, TypeDefField>,

    // type_def
    #[darling(default)]
    namespace: Namespace,

    // serde
    #[darling(default)]
    tag: Option<SpannedValue<String>>,
    #[darling(default)]
    content: Option<SpannedValue<String>>,
    #[darling(default)]
    untagged: SpannedValue<Flag>,
    #[darling(default)]
    rename_all: Option<SpannedValue<String>>,
    #[darling(default)]
    rename: Option<SpannedValue<String>>,
    #[darling(default)]
    #[allow(dead_code)] // doesn't affect JSON
    transparent: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    deny_unknown_fields: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    bound: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    default: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    remote: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    from: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    try_from: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    into: Ignored,
    #[darling(default, rename = "crate")]
    #[allow(dead_code)]
    crate_: Ignored,
}

#[derive(FromField)]
#[darling(attributes(type_def, serde), forward_attrs)]
struct TypeDefField {
    attrs: Vec<Attribute>,
    ident: Option<Ident>,
    ty: Type,

    // type_def
    #[darling(default)]
    type_of: Option<SpannedValue<TypeFromMeta>>,

    // serde
    #[darling(default)]
    flatten: SpannedValue<Flag>,
    #[darling(default)]
    skip_serializing_if: Option<SpannedValue<String>>,
    #[darling(default)]
    default: SpannedValue<FieldDefault>,
    #[darling(default)]
    skip: SpannedValue<Flag>,
    #[darling(default)]
    rename: Option<SpannedValue<String>>,
    #[darling(default)]
    #[allow(dead_code)]
    alias: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    skip_serializing: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    skip_deserializing: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    serialize_with: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    deserialize_with: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    with: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    borrow: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    bound: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    getter: Ignored,
}

#[derive(FromVariant)]
#[darling(attributes(serde), forward_attrs)]
struct TypeDefVariant {
    attrs: Vec<Attribute>,
    ident: Ident,
    fields: ast::Fields<TypeDefField>,

    // serde
    #[darling(default)]
    rename_all: Option<SpannedValue<String>>,
    #[darling(default)]
    skip: SpannedValue<Flag>,
    #[darling(default)]
    rename: Option<SpannedValue<String>>,
    #[darling(default)]
    #[allow(dead_code)]
    alias: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    skip_serializing: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    skip_deserializing: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    serialize_with: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    deserialize_with: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    with: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    bound: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    borrow: Ignored,
    #[darling(default)]
    #[allow(dead_code)]
    other: Ignored,
}

#[derive(Default)]
struct Flag(bool);

#[derive(Default)]
struct Namespace {
    parts: Vec<Ident>,
}

#[derive(Default)]
struct FieldDefault(bool);

struct TypeFromMeta(Type);

fn make_info_def(
    TypeDefInput {
        attrs,
        ident: ty_name,
        generics,
        data,
        namespace,
        tag,
        content,
        untagged,
        rename_all,
        rename,
        ..
    }: &TypeDefInput,
) -> Expr {
    let type_param_decls =
        generics.type_params().flat_map(|TypeParam { ident, .. }| {
            let struct_name = format_ident!("__TypeParam_{}", ident);
            let struct_decl: ItemStruct = parse_quote! {
                #[allow(non_camel_case_types)]
                struct #struct_name;
            };
            let r#ref = type_expr_ident(&ident.to_string());
            let type_def_impl: ItemImpl = parse_quote! {
                impl ::typescript_type_def::TypeDef for #struct_name {
                    const INFO: ::typescript_type_def::type_expr::TypeInfo =
                        ::typescript_type_def::type_expr::TypeInfo::Native(
                            ::typescript_type_def::type_expr::NativeTypeInfo {
                                r#ref: #r#ref,
                            },
                        );
                }
            };
            [Item::Struct(struct_decl), Item::Impl(type_def_impl)]
        });
    let type_info = type_info(
        namespace
            .parts
            .iter()
            .map(|part| type_ident(&part.to_string())),
        &match rename {
            Some(rename) => type_ident(rename.as_str()),
            None => type_ident(&ty_name.unraw().to_string()),
        },
        &match data {
            ast::Data::Struct(ast::Fields { fields, style, .. }) => {
                if let Some(tag) = tag {
                    abort!(tag.span(), "`tag` option is only valid for enums");
                }
                if let Some(content) = content {
                    abort!(
                        content.span(),
                        "`content` option is only valid for enums"
                    );
                }
                if ***untagged {
                    abort!(
                        untagged.span(),
                        "`untagged` option is only valid for enums"
                    );
                }

                match style {
                    ast::Style::Unit => type_expr_ident("null"),
                    ast::Style::Tuple => fields_to_type_expr(
                        fields, false, rename_all, generics, None,
                    ),
                    ast::Style::Struct => {
                        if fields.is_empty() {
                            type_expr_object(
                                [],
                                extract_type_docs(attrs).as_ref(),
                            )
                        } else {
                            fields_to_type_expr(
                                fields,
                                true,
                                rename_all,
                                generics,
                                extract_type_docs(attrs).as_ref(),
                            )
                        }
                    }
                }
            }
            ast::Data::Enum(variants) => variants_to_type_expr(
                variants, tag, content, untagged, rename_all, generics,
            ),
        },
        generics
            .type_params()
            .map(|TypeParam { ident, .. }| type_ident(&ident.to_string())),
        generics.type_params().map(|TypeParam { ident, .. }| {
            type_expr_ref(
                &Type::Path(TypePath {
                    qself: None,
                    path: ident_path(ident.clone()),
                }),
                None,
            )
        }),
        extract_type_docs(attrs).as_ref(),
    );
    parse_quote! {{
        #(#type_param_decls)*
        #type_info
    }}
}

fn fields_to_type_expr(
    fields: &[TypeDefField],
    named: bool,
    rename_all: &Option<SpannedValue<String>>,
    generics: &Generics,
    docs: Option<&Expr>,
) -> Expr {
    if fields.is_empty() {
        return if named {
            type_expr_object(std::iter::empty(), docs)
        } else {
            type_expr_tuple(std::iter::empty(), docs)
        };
    }
    let all_flatten = fields.iter().all(|TypeDefField { flatten, .. }| {
        if ***flatten && !named {
            abort!(flatten.span(), "tuple fields cannot be flattened");
        }
        ***flatten
    });
    let flatten_exprs = fields.iter().filter_map(
        |TypeDefField {
             ty,
             type_of,
             flatten,
             ..
         }| {
            flatten.then(|| {
                let ty = if let Some(type_of) = type_of {
                    &***type_of
                } else {
                    ty
                };
                type_expr_ref(ty, Some(generics))
            })
        },
    );
    // always put flatten exprs first
    let exprs = flatten_exprs.chain((!all_flatten).then(|| {
        // if there are some non-flattened fields, make an expr out of them
        let fields = fields.iter().filter_map(
            |TypeDefField {
                 attrs,
                 ident: field_name,
                 ty,
                 type_of,
                 flatten,
                 skip_serializing_if,
                 default,
                 rename,
                 ..
             }| {
                if ***flatten {
                    if !named {
                        abort!(
                            flatten.span(),
                            "tuple fields cannot be flattened"
                        );
                    }
                    return None;
                }
                let ty = if let Some(type_of) = type_of {
                    &***type_of
                } else {
                    ty
                };
                if let Some(field_name) = field_name {
                    let name = type_string(
                        &serde_rename_ident(
                            field_name, rename, rename_all, true,
                        )
                        .value(),
                        None,
                    );
                    let mut ty = ty;
                    let optional = if let Some(skip_serializing_if) =
                        skip_serializing_if
                    {
                        if let Some(inner_ty) = is_option(ty) {
                            if parse_str::<Path>(skip_serializing_if).unwrap()
                                == parse_str::<Path>("Option::is_none").unwrap()
                            {
                                ty = inner_ty;
                            }
                        }
                        true
                    } else {
                        ***default
                    };
                    let r#type = type_expr_ref(ty, Some(generics));
                    Some(type_object_field(
                        &name,
                        optional,
                        &r#type,
                        extract_type_docs(attrs).as_ref(),
                    ))
                } else {
                    Some(type_expr_ref(ty, Some(generics)))
                }
            },
        );
        if named {
            type_expr_object(fields, docs)
        } else {
            type_expr_tuple(fields, docs)
        }
    }));
    type_expr_intersection(exprs, None)
}

fn variants_to_type_expr(
    variants: &[TypeDefVariant],
    tag: &Option<SpannedValue<String>>,
    content: &Option<SpannedValue<String>>,
    untagged: &SpannedValue<Flag>,
    variant_rename_all: &Option<SpannedValue<String>>,
    generics: &Generics,
) -> Expr {
    type_expr_union(
        variants.iter().map(
            |TypeDefVariant {
                 attrs,
                 ident: variant_name,
                 fields: ast::Fields { style, fields, .. },
                 rename_all: field_rename_all,
                 rename: variant_rename,
                 ..
             }| {
                let variant_name = serde_rename_ident(
                    variant_name,
                    variant_rename,
                    variant_rename_all,
                    false,
                );
                match (tag, content, ***untagged) {
                    (None, None, false) => match style {
                        ast::Style::Unit => type_expr_string(
                            &variant_name.value(),
                            extract_type_docs(attrs).as_ref(),
                        ),
                        ast::Style::Tuple | ast::Style::Struct => {
                            type_expr_object(
                                [type_object_field(
                                    &type_string(&variant_name.value(), None),
                                    false,
                                    &fields_to_type_expr(
                                        fields,
                                        matches!(style, ast::Style::Struct),
                                        field_rename_all,
                                        generics,
                                        None,
                                    ),
                                    extract_type_docs(attrs).as_ref(),
                                )],
                                None,
                            )
                        }
                    },
                    (None, None, true) => match style {
                        ast::Style::Unit => type_expr_ident("null"),
                        ast::Style::Tuple | ast::Style::Struct => {
                            fields_to_type_expr(
                                fields,
                                matches!(style, ast::Style::Struct),
                                field_rename_all,
                                generics,
                                extract_type_docs(attrs).as_ref(),
                            )
                        }
                    },
                    (Some(tag), None, false) => match style {
                        ast::Style::Unit => type_expr_object(
                            [type_object_field(
                                &type_string(&**tag, None),
                                false,
                                &type_expr_string(&variant_name.value(), None),
                                extract_type_docs(attrs).as_ref(),
                            )],
                            None,
                        ),
                        ast::Style::Tuple | ast::Style::Struct => {
                            if matches!(style, ast::Style::Tuple)
                                && fields.len() != 1
                            {
                                abort!(
                                    tag.span(),
                                    "cannot tag enums with tuple variants"
                                );
                            }
                            type_expr_intersection(
                                [
                                    type_expr_object(
                                        [type_object_field(
                                            &type_string(&**tag, None),
                                            false,
                                            &type_expr_string(
                                                &variant_name.value(),
                                                None,
                                            ),
                                            extract_type_docs(attrs).as_ref(),
                                        )],
                                        None,
                                    ),
                                    fields_to_type_expr(
                                        fields,
                                        matches!(style, ast::Style::Struct),
                                        field_rename_all,
                                        generics,
                                        None,
                                    ),
                                ],
                                None,
                            )
                        }
                    },
                    (Some(tag), Some(content), false) => match style {
                        ast::Style::Unit => type_expr_object(
                            [type_object_field(
                                &type_string(&**tag, None),
                                false,
                                &type_expr_string(&variant_name.value(), None),
                                extract_type_docs(attrs).as_ref(),
                            )],
                            None,
                        ),
                        ast::Style::Tuple | ast::Style::Struct => {
                            type_expr_object(
                                [
                                    type_object_field(
                                        &type_string(&**tag, None),
                                        false,
                                        &type_expr_string(
                                            &variant_name.value(),
                                            None,
                                        ),
                                        extract_type_docs(attrs).as_ref(),
                                    ),
                                    type_object_field(
                                        &type_string(&**content, None),
                                        false,
                                        &fields_to_type_expr(
                                            fields,
                                            matches!(style, ast::Style::Struct),
                                            field_rename_all,
                                            generics,
                                            None,
                                        ),
                                        None,
                                    ),
                                ],
                                None,
                            )
                        }
                    },
                    (Some(tag), _, true) => {
                        abort!(
                            tag.span(),
                            "cannot give both `tag` and `untagged` options"
                        );
                    }
                    (None, Some(content), _) => {
                        abort!(
                            content.span(),
                            "`content` option requires `tag` option"
                        );
                    }
                }
            },
        ),
        None,
    )
}

fn type_ident(ident: &str) -> Expr {
    parse_quote! {
        ::typescript_type_def::type_expr::Ident(
            #ident,
        )
    }
}

fn type_string(value: &str, docs: Option<&Expr>) -> Expr {
    let docs = wrap_optional_docs(docs);
    parse_quote! {
        ::typescript_type_def::type_expr::TypeString {
            docs: #docs,
            value: #value,
        }
    }
}

fn type_expr_ident(ident: &str) -> Expr {
    parse_quote! {
        ::typescript_type_def::type_expr::TypeExpr::ident(
            ::typescript_type_def::type_expr::Ident(
                #ident,
            ),
        )
    }
}

fn type_expr_ref(ty: &Type, generics: Option<&Generics>) -> Expr {
    let mut ty = ty.clone();

    if let Some(generics) = generics {
        struct TypeParamReplace<'a> {
            generics: &'a Generics,
        }

        impl VisitMut for TypeParamReplace<'_> {
            fn visit_type_path_mut(&mut self, type_path: &mut TypePath) {
                let TypePath { path, .. } = type_path;
                if let Some(TypeParam { ident, .. }) = self
                    .generics
                    .type_params()
                    .find(|TypeParam { ident, .. }| path.is_ident(ident))
                {
                    *path = ident_path(format_ident!("__TypeParam_{}", ident));
                }

                visit_mut::visit_type_path_mut(self, type_path);
            }
        }

        visit_mut::visit_type_mut(&mut TypeParamReplace { generics }, &mut ty);
    }

    parse_quote! {
        ::typescript_type_def::type_expr::TypeExpr::Ref(
            &<#ty as ::typescript_type_def::TypeDef>::INFO,
        )
    }
}

fn type_expr_string(value: &str, docs: Option<&Expr>) -> Expr {
    let docs = wrap_optional_docs(docs);
    parse_quote! {
        ::typescript_type_def::type_expr::TypeExpr::String(
            ::typescript_type_def::type_expr::TypeString {
                docs: #docs,
                value: #value,
            },
        )
    }
}

fn type_expr_tuple(
    exprs: impl IntoIterator<Item = Expr>,
    docs: Option<&Expr>,
) -> Expr {
    let docs = wrap_optional_docs(docs);
    let exprs = exprs.into_iter().collect::<Vec<_>>();
    if exprs.len() == 1 {
        exprs.into_iter().next().unwrap()
    } else {
        parse_quote! {
            ::typescript_type_def::type_expr::TypeExpr::Tuple(
                ::typescript_type_def::type_expr::TypeTuple {
                    docs: #docs,
                    elements: &[#(#exprs,)*],
                },
            )
        }
    }
}

fn type_object_field(
    name: &Expr,
    optional: bool,
    r#type: &Expr,
    docs: Option<&Expr>,
) -> Expr {
    let docs = wrap_optional_docs(docs);
    parse_quote! {
        ::typescript_type_def::type_expr::ObjectField {
            docs: #docs,
            name: #name,
            optional: #optional,
            r#type: #r#type,
        }
    }
}

fn type_expr_object(
    exprs: impl IntoIterator<Item = Expr>,
    docs: Option<&Expr>,
) -> Expr {
    let docs = wrap_optional_docs(docs);
    let exprs = exprs.into_iter();
    parse_quote! {
        ::typescript_type_def::type_expr::TypeExpr::Object(
            ::typescript_type_def::type_expr::TypeObject {
                docs: #docs,
                index_signature: ::core::option::Option::None,
                fields: &[#(#exprs,)*],
            },
        )
    }
}

fn type_expr_union(
    exprs: impl IntoIterator<Item = Expr>,
    docs: Option<&Expr>,
) -> Expr {
    let docs = wrap_optional_docs(docs);
    let exprs = exprs.into_iter().collect::<Vec<_>>();
    if exprs.len() == 1 {
        exprs.into_iter().next().unwrap()
    } else {
        parse_quote! {
            ::typescript_type_def::type_expr::TypeExpr::Union(
                ::typescript_type_def::type_expr::TypeUnion {
                    docs: #docs,
                    members: &[#(#exprs,)*],
                },
            )
        }
    }
}

fn type_expr_intersection(
    exprs: impl IntoIterator<Item = Expr>,
    docs: Option<&Expr>,
) -> Expr {
    let docs = wrap_optional_docs(docs);
    let exprs = exprs.into_iter().collect::<Vec<_>>();
    if exprs.len() == 1 {
        exprs.into_iter().next().unwrap()
    } else {
        parse_quote! {
            ::typescript_type_def::type_expr::TypeExpr::Intersection(
                ::typescript_type_def::type_expr::TypeIntersection {
                    docs: #docs,
                    members: &[#(#exprs,)*],
                },
            )
        }
    }
}

fn type_info(
    path_parts: impl IntoIterator<Item = Expr>,
    name: &Expr,
    def: &Expr,
    generic_vars: impl IntoIterator<Item = Expr>,
    generic_args: impl IntoIterator<Item = Expr>,
    docs: Option<&Expr>,
) -> Expr {
    let docs = wrap_optional_docs(docs);
    let path_parts = path_parts.into_iter();
    let generic_vars = generic_vars.into_iter();
    let generic_args = generic_args.into_iter();
    parse_quote! {
        ::typescript_type_def::type_expr::TypeInfo::Defined(
            ::typescript_type_def::type_expr::DefinedTypeInfo {
                def: ::typescript_type_def::type_expr::TypeDefinition {
                    docs: #docs,
                    path: &[#(#path_parts,)*],
                    name: #name,
                    generic_vars: &[#(#generic_vars,)*],
                    def: #def,
                },
                generic_args: &[#(#generic_args,)*],
            },
        )
    }
}

fn extract_type_docs(attrs: &[Attribute]) -> Option<Expr> {
    let mut lines = attrs
        .iter()
        .filter_map(|attr| {
            if let Ok(Meta::NameValue(MetaNameValue {
                path,
                lit: Lit::Str(lit_str),
                ..
            })) = attr.parse_meta()
            {
                path.is_ident("doc").then(|| lit_str.value())
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    let min_indent = lines
        .iter()
        .filter_map(|line| {
            if line.is_empty() {
                None
            } else {
                Some(
                    line.find(|c: char| !c.is_whitespace())
                        .unwrap_or_else(|| line.len()),
                )
            }
        })
        .min()?;
    if min_indent > 0 {
        for line in &mut lines {
            if !line.is_empty() {
                *line = line.split_off(min_indent);
            }
        }
    }
    let docs = lines.join("\n");
    Some(parse_quote! {
        ::typescript_type_def::type_expr::Docs(
            #docs,
        )
    })
}

fn wrap_optional_docs(docs: Option<&Expr>) -> Expr {
    match docs {
        Some(docs) => parse_quote! {
            ::core::option::Option::Some(
                #docs,
            )
        },
        None => parse_quote! {
            ::core::option::Option::None
        },
    }
}

fn serde_rename_ident(
    ident: &Ident,
    rename: &Option<SpannedValue<String>>,
    rename_all: &Option<SpannedValue<String>>,
    is_field: bool,
) -> LitStr {
    let span = ident.span();
    if let Some(rename) = rename {
        LitStr::new(rename.as_str(), span)
    } else {
        let ident = ident.unraw().to_string();
        let ident = if let Some(rename_all) = rename_all {
            match rename_all.as_str() {
                "lowercase" => ident.to_lowercase(),
                "UPPERCASE" => ident.to_uppercase(),
                _ => match ident_case::RenameRule::from_str(rename_all) {
                    Ok(rename_all) => match is_field {
                        true => rename_all.apply_to_field(ident),
                        false => rename_all.apply_to_variant(ident),
                    },
                    Err(()) => {
                        abort!(rename_all.span(), "unknown case conversion")
                    }
                },
            }
        } else {
            ident
        };
        LitStr::new(&ident, span)
    }
}

fn remove_skipped(data: &mut ast::Data<TypeDefVariant, TypeDefField>) {
    match data {
        ast::Data::Struct(ast::Fields { fields, .. }) => {
            remove_if(fields, |TypeDefField { skip, .. }| ***skip);
        }
        ast::Data::Enum(variants) => {
            remove_if(
                variants,
                |TypeDefVariant {
                     fields: ast::Fields { fields, .. },
                     skip,
                     ..
                 }| {
                    if ***skip {
                        return true;
                    }
                    remove_if(fields, |TypeDefField { skip, .. }| ***skip);
                    false
                },
            );
        }
    }
}

impl Deref for Flag {
    type Target = bool;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl FromMeta for Flag {
    fn from_word() -> Result<Self, darling::Error> {
        Ok(Self(true))
    }
}

impl FromMeta for Namespace {
    fn from_value(value: &Lit) -> Result<Self, darling::Error> {
        match value {
            Lit::Str(lit_str) => Ok(Self {
                parts: lit_str
                    .value()
                    .split('.')
                    .map(|part| format_ident!("{}", part))
                    .collect(),
            }),
            _ => Err(darling::Error::custom("expected string literal")),
        }
    }
}

impl Deref for FieldDefault {
    type Target = bool;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl FromMeta for FieldDefault {
    fn from_word() -> Result<Self, darling::Error> {
        Ok(Self(true))
    }

    fn from_string(_value: &str) -> Result<Self, darling::Error> {
        Ok(Self(true))
    }
}

impl Deref for TypeFromMeta {
    type Target = Type;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl FromMeta for TypeFromMeta {
    fn from_string(value: &str) -> Result<Self, darling::Error> {
        parse_str(value).map(Self).map_err(Into::into)
    }
}

fn is_option(ty: &Type) -> Option<&Type> {
    if let Type::Path(TypePath {
        qself: None,
        path:
            Path {
                leading_colon: None,
                segments,
            },
    }) = ty
    {
        if segments.len() == 1 {
            let PathSegment { ident, arguments } = &segments[0];
            if ident == "Option" {
                if let PathArguments::AngleBracketed(
                    AngleBracketedGenericArguments { args, .. },
                ) = arguments
                {
                    if args.len() == 1 {
                        if let GenericArgument::Type(ty) = &args[0] {
                            return Some(ty);
                        }
                    }
                }
            }
        }
    }
    None
}

fn ident_path(ident: Ident) -> Path {
    let mut segments = Punctuated::new();
    segments.push_value(PathSegment {
        ident,
        arguments: PathArguments::None,
    });
    Path {
        leading_colon: None,
        segments,
    }
}

fn remove_if<T, F>(vec: &mut Vec<T>, mut filter: F)
where
    F: FnMut(&mut T) -> bool,
{
    let mut i = 0;
    while i < vec.len() {
        if filter(&mut vec[i]) {
            vec.remove(i);
        } else {
            i += 1;
        }
    }
}
