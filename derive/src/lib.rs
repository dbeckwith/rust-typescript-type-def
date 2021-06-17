//! This crate defines a procedural derive macro for implementing the `TypeDef`
//! trait from the `typescript_type_def` crate.
//!
//! See the documentation of that crate for more information.
#![warn(rust_2018_idioms, clippy::all)]
#![deny(clippy::correctness)]

use darling::{
    ast,
    util::SpannedValue,
    FromDeriveInput,
    FromField,
    FromMeta,
    FromVariant,
};
use proc_macro2::{Span, TokenStream};
use proc_macro_error::{abort, abort_call_site, proc_macro_error};
use quote::{format_ident, quote, ToTokens};
use std::{iter, str::FromStr};
use syn::{
    ext::IdentExt,
    parse_quote,
    parse_str,
    punctuated::Punctuated,
    AngleBracketedGenericArguments,
    Attribute,
    Binding,
    DeriveInput,
    Expr,
    GenericArgument,
    Generics,
    Ident,
    Lifetime,
    Lit,
    LitStr,
    Meta,
    MetaNameValue,
    ParenthesizedGenericArguments,
    Path,
    PathArguments,
    PathSegment,
    ReturnType,
    Token,
    Type,
    TypePath,
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

    data_to_static(&mut input.data);

    let ty_name = &input.ident;

    let info_def = make_info_def(&input);

    let ty_generics = {
        let generics = input.generics;
        if generics.type_params().next().is_some()
            || generics.const_params().next().is_some()
        {
            abort_call_site!("cannot derive TypeDef for generic types");
        }
        // replace any lifetimes with 'static
        let ty_generics = generics
            .lifetimes()
            .map(|_| Lifetime::new("'static", Span::call_site()))
            .collect::<Punctuated<_, Token![,]>>();
        if ty_generics.is_empty() {
            TokenStream::new()
        } else {
            let mut tokens = TokenStream::new();
            <Token![<]>::default().to_tokens(&mut tokens);
            ty_generics.to_tokens(&mut tokens);
            <Token![>]>::default().to_tokens(&mut tokens);
            tokens
        }
    };

    (quote! {
        impl ::typescript_type_def::TypeDef for #ty_name
        #ty_generics
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
    #[darling(default)]
    namespace: Namespace,
    #[darling(default)]
    tag: Option<SpannedValue<String>>,
    #[darling(default)]
    content: Option<SpannedValue<String>>,
    #[darling(default)]
    untagged: SpannedValue<bool>,
    #[darling(default)]
    rename_all: Option<SpannedValue<String>>,
    #[darling(default)]
    #[allow(dead_code)] // doesn't affect JSON
    transparent: SpannedValue<bool>,
}

#[derive(FromField)]
#[darling(attributes(serde), forward_attrs)]
struct TypeDefField {
    attrs: Vec<Attribute>,
    ident: Option<Ident>,
    ty: Type,
    #[darling(default)]
    flatten: SpannedValue<bool>,
    #[darling(default)]
    skip_serializing_if: Option<SpannedValue<String>>,
    #[darling(default)]
    default: SpannedValue<bool>,
}

#[derive(FromVariant)]
#[darling(attributes(serde), forward_attrs)]
struct TypeDefVariant {
    attrs: Vec<Attribute>,
    ident: Ident,
    fields: ast::Fields<TypeDefField>,
    #[darling(default)]
    rename_all: Option<SpannedValue<String>>,
}

#[derive(Default)]
struct Namespace {
    parts: Vec<Ident>,
}

fn make_info_def(
    TypeDefInput {
        attrs,
        ident: ty_name,
        data,
        namespace,
        tag,
        content,
        untagged,
        rename_all,
        ..
    }: &TypeDefInput,
) -> Expr {
    type_info(
        &type_name(
            namespace
                .parts
                .iter()
                .map(|part| type_ident(&part.to_string())),
            &type_ident(&ty_name.unraw().to_string()),
        ),
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
                if **untagged {
                    abort!(
                        untagged.span(),
                        "`untagged` option is only valid for enums"
                    );
                }

                match style {
                    ast::Style::Unit => type_expr_ident("null"),
                    ast::Style::Tuple => {
                        fields_to_type_expr(fields, rename_all, None)
                    },
                    ast::Style::Struct => {
                        if fields.is_empty() {
                            type_expr_object(
                                iter::empty(),
                                extract_type_docs(attrs).as_ref(),
                            )
                        } else {
                            let all_flatten = fields
                                .iter()
                                .all(|TypeDefField { flatten, .. }| **flatten);
                            type_expr_intersection(
                            fields
                                .iter()
                                .filter_map(
                                    |TypeDefField { ty, flatten, .. }| {
                                        flatten.then(|| type_expr_ref(ty))
                                    },
                                )
                                .chain((!all_flatten).then(|| {
                                    fields_to_type_expr(
                                        fields,
                                        rename_all,
                                        extract_type_docs(attrs).as_ref(),
                                    )
                                })),
                            None,
                        )
                        }
                    },
                }
            },
            ast::Data::Enum(variants) => variants_to_type_expr(
                variants, tag, content, untagged, rename_all,
            ),
        },
        extract_type_docs(attrs).as_ref(),
    )
}

fn fields_to_type_expr(
    fields: &[TypeDefField],
    rename: &Option<SpannedValue<String>>,
    docs: Option<&Expr>,
) -> Expr {
    let named = fields.first().unwrap().ident.is_some();
    let fields = fields.iter().filter_map(
        |TypeDefField {
             attrs,
             ident: field_name,
             ty,
             flatten,
             skip_serializing_if,
             default,
             ..
         }| {
            if **flatten {
                if !named {
                    abort!(flatten.span(), "tuple fields cannot be flattened");
                }
                return None;
            }
            let mut ty = ty;
            if let Some(field_name) = field_name {
                let name = type_string(
                    &serde_rename_ident(field_name, rename, true).value(),
                    None,
                );
                let optional =
                    if let Some(skip_serializing_if) = skip_serializing_if {
                        if let Some(inner_ty) = is_option(ty) {
                            if parse_str::<Path>(skip_serializing_if).unwrap()
                                == parse_str::<Path>("Option::is_none").unwrap()
                            {
                                ty = inner_ty;
                            }
                        }
                        true
                    } else {
                        **default
                    };
                let r#type = type_expr_ref(ty);
                Some(type_object_field(
                    &name,
                    optional,
                    &r#type,
                    extract_type_docs(attrs).as_ref(),
                ))
            } else {
                Some(type_expr_ref(ty))
            }
        },
    );
    if named {
        type_expr_object(fields, docs)
    } else {
        type_expr_tuple(fields, docs)
    }
}

fn variants_to_type_expr(
    variants: &[TypeDefVariant],
    tag: &Option<SpannedValue<String>>,
    content: &Option<SpannedValue<String>>,
    untagged: &SpannedValue<bool>,
    variant_rename: &Option<SpannedValue<String>>,
) -> Expr {
    type_expr_union(
        variants.iter().map(
            |TypeDefVariant {
                 attrs,
                 ident: variant_name,
                 fields: ast::Fields { style, fields, .. },
                 rename_all: field_rename,
                 ..
             }| {
                let variant_name =
                    serde_rename_ident(variant_name, variant_rename, false);
                match (tag, content, **untagged) {
                    (None, None, false) => match style {
                        ast::Style::Unit => type_expr_string(
                            &variant_name.value(),
                            extract_type_docs(attrs).as_ref(),
                        ),
                        ast::Style::Tuple | ast::Style::Struct => {
                            type_expr_object(
                                iter::once(type_object_field(
                                    &type_string(&variant_name.value(), None),
                                    false,
                                    &fields_to_type_expr(
                                        fields,
                                        field_rename,
                                        None,
                                    ),
                                    extract_type_docs(attrs).as_ref(),
                                )),
                                None,
                            )
                        },
                    },
                    (None, None, true) => match style {
                        ast::Style::Unit => type_expr_ident("null"),
                        ast::Style::Tuple | ast::Style::Struct => {
                            fields_to_type_expr(
                                fields,
                                field_rename,
                                extract_type_docs(attrs).as_ref(),
                            )
                        },
                    },
                    (Some(tag), None, false) => match style {
                        ast::Style::Unit => type_expr_object(
                            iter::once(type_object_field(
                                &type_string(&**tag, None),
                                false,
                                &type_expr_string(&variant_name.value(), None),
                                extract_type_docs(attrs).as_ref(),
                            )),
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
                                std::array::IntoIter::new([
                                    type_expr_object(
                                        iter::once(type_object_field(
                                            &type_string(&**tag, None),
                                            false,
                                            &type_expr_string(
                                                &variant_name.value(),
                                                None,
                                            ),
                                            extract_type_docs(attrs).as_ref(),
                                        )),
                                        None,
                                    ),
                                    fields_to_type_expr(
                                        fields,
                                        field_rename,
                                        None,
                                    ),
                                ]),
                                None,
                            )
                        },
                    },
                    (Some(tag), Some(content), false) => match style {
                        ast::Style::Unit => type_expr_object(
                            iter::once(type_object_field(
                                &type_string(&**tag, None),
                                false,
                                &type_expr_string(&variant_name.value(), None),
                                extract_type_docs(attrs).as_ref(),
                            )),
                            None,
                        ),
                        ast::Style::Tuple | ast::Style::Struct => {
                            type_expr_object(
                                std::array::IntoIter::new([
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
                                            field_rename,
                                            None,
                                        ),
                                        None,
                                    ),
                                ]),
                                None,
                            )
                        },
                    },
                    (Some(tag), _, true) => {
                        abort!(
                            tag.span(),
                            "cannot give both `tag` and `untagged` options"
                        );
                    },
                    (None, Some(content), _) => {
                        abort!(
                            content.span(),
                            "`content` option requires `tag` option"
                        );
                    },
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

fn type_expr_ref(ty: &Type) -> Expr {
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

fn type_name(path_parts: impl Iterator<Item = Expr>, name: &Expr) -> Expr {
    parse_quote! {
        ::typescript_type_def::type_expr::TypeName {
            path: &[#(#path_parts,)*],
            name: #name,
            generics: &[],
        }
    }
}

fn type_expr_tuple(
    exprs: impl Iterator<Item = Expr>,
    docs: Option<&Expr>,
) -> Expr {
    let docs = wrap_optional_docs(docs);
    let exprs = exprs.collect::<Vec<_>>();
    if exprs.len() == 1 {
        exprs.into_iter().next().unwrap()
    } else {
        parse_quote! {
            ::typescript_type_def::type_expr::TypeExpr::Tuple(
                ::typescript_type_def::type_expr::Tuple {
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
    exprs: impl Iterator<Item = Expr>,
    docs: Option<&Expr>,
) -> Expr {
    let docs = wrap_optional_docs(docs);
    parse_quote! {
        ::typescript_type_def::type_expr::TypeExpr::Object(
            ::typescript_type_def::type_expr::Object {
                docs: #docs,
                fields: &[#(#exprs,)*],
            },
        )
    }
}

fn type_expr_union(
    exprs: impl Iterator<Item = Expr>,
    docs: Option<&Expr>,
) -> Expr {
    let docs = wrap_optional_docs(docs);
    let exprs = exprs.collect::<Vec<_>>();
    if exprs.len() == 1 {
        exprs.into_iter().next().unwrap()
    } else {
        parse_quote! {
            ::typescript_type_def::type_expr::TypeExpr::Union(
                ::typescript_type_def::type_expr::Union {
                    docs: #docs,
                    members: &[#(#exprs,)*],
                },
            )
        }
    }
}

fn type_expr_intersection(
    exprs: impl Iterator<Item = Expr>,
    docs: Option<&Expr>,
) -> Expr {
    let docs = wrap_optional_docs(docs);
    let exprs = exprs.collect::<Vec<_>>();
    if exprs.len() == 1 {
        exprs.into_iter().next().unwrap()
    } else {
        parse_quote! {
            ::typescript_type_def::type_expr::TypeExpr::Intersection(
                ::typescript_type_def::type_expr::Intersection {
                    docs: #docs,
                    members: &[#(#exprs,)*],
                },
            )
        }
    }
}

fn type_info(name: &Expr, def: &Expr, docs: Option<&Expr>) -> Expr {
    let docs = wrap_optional_docs(docs);
    parse_quote! {
        ::typescript_type_def::type_expr::TypeInfo::Defined(
            ::typescript_type_def::type_expr::DefinedTypeInfo {
                docs: #docs,
                name: #name,
                def: #def,
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
    is_field: bool,
) -> LitStr {
    LitStr::new(
        &{
            let ident = ident.unraw().to_string();
            if let Some(rename) = rename {
                match rename.as_str() {
                    "lowercase" => ident.to_lowercase(),
                    "UPPERCASE" => ident.to_uppercase(),
                    _ => match ident_case::RenameRule::from_str(rename) {
                        Ok(rename) => match is_field {
                            true => rename.apply_to_field(ident),
                            false => rename.apply_to_variant(ident),
                        },
                        Err(()) => {
                            abort!(rename.span(), "unknown case conversion")
                        },
                    },
                }
            } else {
                ident
            }
        },
        ident.span(),
    )
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

fn data_to_static(data: &mut ast::Data<TypeDefVariant, TypeDefField>) {
    match data {
        ast::Data::Struct(ast::Fields { fields, .. }) => {
            for TypeDefField { ty, .. } in fields {
                ty_to_static(ty);
            }
        },
        ast::Data::Enum(variants) => {
            for TypeDefVariant {
                fields: ast::Fields { fields, .. },
                ..
            } in variants
            {
                for TypeDefField { ty, .. } in fields {
                    ty_to_static(ty);
                }
            }
        },
    }
}

fn ty_to_static(ty: &mut Type) {
    if let Type::Path(TypePath {
        path: Path { segments, .. },
        ..
    }) = ty
    {
        for PathSegment { arguments, .. } in segments {
            match arguments {
                PathArguments::None => {},
                PathArguments::AngleBracketed(
                    AngleBracketedGenericArguments { args, .. },
                ) => {
                    for arg in args {
                        match arg {
                            GenericArgument::Lifetime(lifetime) => {
                                *lifetime =
                                    Lifetime::new("'static", Span::call_site());
                            },
                            GenericArgument::Type(ty) => ty_to_static(ty),
                            GenericArgument::Binding(Binding {
                                ty, ..
                            }) => ty_to_static(ty),
                            GenericArgument::Constraint(_)
                            | GenericArgument::Const(_) => {},
                        }
                    }
                },
                PathArguments::Parenthesized(
                    ParenthesizedGenericArguments { inputs, output, .. },
                ) => {
                    for input in inputs {
                        ty_to_static(input);
                    }
                    match output {
                        ReturnType::Default => {},
                        ReturnType::Type(_, ty) => ty_to_static(ty),
                    }
                },
            }
        }
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
